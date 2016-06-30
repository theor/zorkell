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

instance Show ObjectNumber where
  show (ObjectNumber o) = printf "0x%02x" o
-- In Versions 1 to 3, there are at most 255 objects, each having a 9-byte entry as follows:
-- the 32 attribute flags     parent     sibling     child   properties
-- --32 bits in 4 bytes---   ---3 bytes------------------  ---2 bytes--
data Object = Object { flags :: Word32
                     , parent :: ObjectNumber
                     , sibling :: ObjectNumber
                     , child :: ObjectNumber
                     , properties :: ByteAddr }
                     deriving (Show)

--text-length     text of short name of object
-----byte----   --some even number of bytes---
type PropertyHeader = (Int, String)
readPropHeader :: ByteAddr -> BS.Get PropertyHeader
readPropHeader o = do
  BS.skip . toInt $ o
  len <- (2*) . fromIntegral  <$> BS.getWord8
  str <- BS.getByteString len
  return (len, ZString.decodeString str)

readObject :: ByteAddr -> Int -> BS.Get Object
readObject h on = do
  BS.skip (toInt h)
  BS.skip $ (62) + (9 * (on - 1))
  f <- BS.getWord32be
  p <- ObjectNumber <$> BS.getWord8
  s <- ObjectNumber <$> BS.getWord8
  c <- ObjectNumber <$> BS.getWord8
  props <- getByteAddr
  return $ Object f p s c props


readObjectTable (ByteAddr a) = do
  BS.skip 31 -- property defaults table. 31 words in v1-3, 63 in v4+
