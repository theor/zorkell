module ObjectTable where

import Control.Monad
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

import Types
import BinUtils
import Header
import qualified ZString
import Text.Printf

import Debug.Trace

newtype ObjectNumber = ObjectNumber Word8 deriving (Eq)
invalidObject = (==) $ ObjectNumber 0

instance Show ObjectNumber where
  show (ObjectNumber o) = printf "0x%02x" o
-- In Versions 1 to 3, there are at most 255 objects, each having a 9-byte entry as follows:
-- the 32 attribute flags     parent     sibling     child   properties
-- --32 bits in 4 bytes---   ---3 bytes------------------  ---2 bytes--
data ObjectHeader = ObjectHeader { flags :: Word32
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
readPropHeader :: ByteAddr -> BS.Get PropertyHeader
readPropHeader o = do
  BS.skip . toInt $ o
  len <- (2*) . fromIntegral  <$> BS.getWord8
  str <- BS.getByteString len
  return (len, if len == 0 then "<unnamed>" else ZString.decodeString str)

objectAddr :: ByteAddr -> Int -> Int
objectAddr h on = toInt h + 62 + 9 * (on - 1)

readObjectHeader :: ByteAddr -> Int -> BS.Get ObjectHeader
readObjectHeader h on = do
  BS.skip $ objectAddr h on
  f <- BS.getWord32be
  p <- ObjectNumber <$> BS.getWord8
  s <- ObjectNumber <$> BS.getWord8
  c <- ObjectNumber <$> BS.getWord8
  props <- getByteAddr
  return $ ObjectHeader f p s c props

readObject addr on = do
  h <- readObjectHeader addr on
  -- BS.skip
  return h

readAllObjects :: ByteAddr -> BS.Get [ObjectHeader]
readAllObjects a = return []

objectCount :: ByteAddr -> BS.Get Int
objectCount a = do
  fstObj <- readObjectHeader a 1
  let fstPropTable = properties fstObj
      fstObjAddr = objectAddr a 1
      delta = toInt fstPropTable - fstObjAddr
  traceM "OBJECT OUNT !!!!"
  traceShowM (fstPropTable, fstObjAddr, delta)
  return $ delta `div` 9
  -- fromIntegral <$> BS.getWord8

readObjectTable (ByteAddr a) =
  BS.skip 31 -- property defaults table. 31 words in v1-3, 63 in v4+
