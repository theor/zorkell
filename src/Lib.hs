module Lib where

import Text.Printf
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

newtype ByteAddr = ByteAddr Word16 -- deriving (Show)
instance Show ByteAddr where
  show (ByteAddr b) = printf "0x%04X" b
b = ByteAddr
data Header = Header {
                  version :: Word8 -- 0
                , flags1 :: Word8 -- 1
                , baseAddr :: ByteAddr -- 4
                , initPc :: ByteAddr -- 6
                , dictionaryLoc :: ByteAddr -- 8
                , objectTableLoc :: ByteAddr -- A
                , globalVarLoc :: ByteAddr -- C
                , baseStaticAddr :: ByteAddr -- E
                , flags2 :: Word8 -- 10
                , abbrevLoc :: ByteAddr -- 18
                , length :: Word16 -- 1A
                , checksum :: Word16 -- 1C
              }
  deriving (Show)

getWord16 = BS.getWord16be
getByteAddr = ByteAddr <$> getWord16
f @@ i = do
  bytesRead <- BS.bytesRead
  if bytesRead == i then f else fail $ printf "mismatch, should be at byte 0x%02x, is at 0x%02x" i bytesRead
readStory :: t -> BS.Get Header
readStory x = do
  v <- BS.getWord8              @@ 0x0
  flags1 <- BS.getWord8         @@ 0x1
  BS.skip 2                     @@ 0x2
  base <- getByteAddr           @@ 0x4
  initPc <- getByteAddr         @@ 0x6
  dictionaryLoc <- getByteAddr  @@ 0x8
  objectTableLoc <- getByteAddr @@ 0xA
  globalVarLoc <- getByteAddr   @@ 0xC
  baseStaticAddr <- getByteAddr @@ 0xE
  flags2 <- BS.getWord8         @@ 0x10
  BS.skip 7
  abbrevLoc <- getByteAddr      @@ 0x18
  length <- getWord16           @@ 0x1A
  checksum <- getWord16         @@ 0x1C
  
  return $ Header v flags1 base initPc dictionaryLoc
    objectTableLoc globalVarLoc baseStaticAddr flags2 abbrevLoc
    length checksum
