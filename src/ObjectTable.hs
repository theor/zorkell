{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ObjectTable where

import Control.Monad
import Control.Monad.State
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

import Types
import BinUtils
import Header
import qualified ZString
import Text.Printf

import Debug.Trace

newtype ObjectNumber = ObjectNumber Word8
  deriving (Eq, Ord, Num, Integral, Real, Enum)
-- instance Integral ObjectNumber where
--   fromIntegral o = 0

invalidObject :: ObjectNumber -> Bool
invalidObject = (==) $ ObjectNumber 0

instance Show ObjectNumber where
  show (ObjectNumber o) = printf "0x%02x" o

-- In Versions 1 to 3, there are at most 255 objects, each having a 9-byte entry as follows:
-- the 32 attribute flags     parent     sibling     child   properties
-- --32 bits in 4 bytes---   ---3 bytes------------------  ---2 bytes--
data ObjectHeader = ObjectHeader { flags :: Word32
                                 , oid :: ObjectNumber
                                 , parent :: ObjectNumber
                                 , sibling :: ObjectNumber
                                 , child :: ObjectNumber
                                 , properties :: ByteAddr }
                                 deriving (Show, Eq)
data Object = Object { header :: ObjectHeader
                     , shortNameLen :: Int
                     , shortName :: String }
                     deriving (Show, Eq)

--text-length     text of short name of object
-----byte----   --some even number of bytes---
type PropertyHeader = (Int, String)

readPropHeader :: StoryReader PropertyHeader
readPropHeader = do
  (s,_) <- get
  len <- exec $ (2 *) . fromIntegral <$> BS.getWord8
  name <- if len == 0 then return "<unnamed>" else ZString.decodeString
  return (len, name)

objectAddr :: ByteAddr -> Int -> Int
-- property defaults table. 31 words in v1-3, 63 in v4+
objectAddr h on = toInt h + 62 + 9 * (on - 1)

getObjectAddr :: Int -> StoryReader Int
getObjectAddr n = do
  header <- getHeader
  return $ objectAddr (objectTableLoc header) n


readObjectHeader :: Int -> StoryReader ObjectHeader
readObjectHeader on = do
  addr <- getObjectAddr on
  setAt addr
  exec $ do
    f <- BS.getWord32be
    p <- ObjectNumber <$> BS.getWord8
    s <- ObjectNumber <$> BS.getWord8
    c <- ObjectNumber <$> BS.getWord8
    props <- getByteAddr
    return $ ObjectHeader f (ObjectNumber . fromIntegral $ on) p s c props

readObject :: Int -> StoryReader Object
readObject i = do obj <- readObjectHeader i
                  setAtAddr $ properties obj
                  (nameLen,nameStr) <- readPropHeader
                  return $ Object obj nameLen nameStr

readAllObjects :: StoryReader [Object]
readAllObjects = do
  header <- getHeader
  count <- objectCount
  let i = [1,2..count]
  mapM readObject i

objectCount :: StoryReader Int
objectCount = do
  fstObj <- readObjectHeader 1
  let fstPropTable = properties fstObj
  fstObjAddr <- getObjectAddr 1
  let delta = toInt fstPropTable - fstObjAddr
  -- traceShowM (fstPropTable, fstObjAddr, delta)
  return $ delta `div` 9
