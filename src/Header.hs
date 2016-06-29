module Header where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

import Types
import BinUtils
import Text.Printf

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

data Story = Story {
    header :: Header
  , dynMem :: B.ByteString
  , staticMem :: B.ByteString
} deriving (Show)

readHeader :: BS.Get Header
readHeader = do
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

readStory :: B.ByteString -> Either String Story
readStory bstr = do
  let (eheader,_) = BS.runGet readHeader bstr
  header <- eheader
  let staticOffset = toInt . baseStaticAddr $ header
  let (dyn,stat) = B.splitAt staticOffset bstr
  return $ Story header dyn stat -- (B.take staticOffset (B.singleton 0)
