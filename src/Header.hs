module Header where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

import Types
import BinUtils

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
  bflags1 <- BS.getWord8         @@ 0x1
  BS.skip 2                     @@ 0x2
  base <- getByteAddr           @@ 0x4
  binitPc <- getByteAddr         @@ 0x6
  bdictionaryLoc <- getByteAddr  @@ 0x8
  bobjectTableLoc <- getByteAddr @@ 0xA
  bglobalVarLoc <- getByteAddr   @@ 0xC
  bbaseStaticAddr <- getByteAddr @@ 0xE
  bflags2 <- BS.getWord8         @@ 0x10
  BS.skip 7
  babbrevLoc <- getByteAddr      @@ 0x18
  blength <- getWord16           @@ 0x1A
  bchecksum <- getWord16         @@ 0x1C

  return $ Header v bflags1 base binitPc bdictionaryLoc
    bobjectTableLoc bglobalVarLoc bbaseStaticAddr bflags2 babbrevLoc
    blength bchecksum

readStory :: B.ByteString -> Either String Story
readStory bstr = do
  let (eheader,_) = BS.runGet readHeader bstr
  vheader <- eheader
  let staticOffset = toInt . baseStaticAddr $ vheader
  let (dyn,stat) = B.splitAt staticOffset bstr
  return $ Story vheader dyn stat -- (B.take staticOffset (B.singleton 0)
