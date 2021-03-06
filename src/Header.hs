module Header where

import Debug.Trace

import Text.Printf
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
import qualified BinReader as BR
import Control.Monad.State
import Types
import BinUtils

newtype Version = Version Int
  deriving (Show, Eq)
data Header = Header {
                  version :: Version -- 0
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
}

instance Show Story where
  show s = printf "Story { header = %s, dynMem = %i, staticMem = %i }" (show . header $ s) (B.length . dynMem $ s) (B.length . staticMem $ s)

type StoryReader a = StateT (Story,Int) (Either String) a
-- getWord8 :: StoryReader a
-- getWord8 = lift BS.getWord8

isAt :: StoryReader Int
isAt = do x <- get
          return $ snd x -- lift $ return 1

setAt :: Int -> StoryReader ()
setAt x = do
   (story,offset) <- get
   put (story, x)
setAtAddr :: ByteAddr -> StoryReader ()
setAtAddr x = do
  (story,offset) <- get
  put (story, toInt x)

-- exec :: BS.Get a -> StoryReader a
exec x = do
  (story,offset) <- get
  -- traceShowM ("cur offset", offset)
  let staticStart = toInt . baseStaticAddr . header $ story
  let isDynMem = offset < staticStart
  let r = do
             bread <- BS.bytesRead
             rema <- BS.remaining
            --  traceShowM ("will skip", offset, "read", bread, "remaining", rema)
             BS.skip $ if isDynMem then offset else offset - staticStart
            --  traceShowM ("Skipped", offset)
             xx <- x
             bread <- (+ if isDynMem then 0 else staticStart) <$> BS.bytesRead
             return (bread,xx)
  let yy = fst . BS.runGet r . (if isDynMem then dynMem else staticMem) $ story
  setAt $ either (const $ trace "ERROR SET" offset) fst yy
  lift (fmap snd yy)
--
run :: Either String Story -> StoryReader a -> Either String a
run story x = do
  s <- story
  y <- runStateT x (s, 0)
  return $ fst y

getHeader :: StoryReader Header
getHeader = do
  (s,_) <- get
  return $ header s

readHeader :: BS.Get Header
readHeader = do
  v <- Version . fromIntegral <$> BS.getWord8 @@ 0x0
  bflags1 <- BS.getWord8                      @@ 0x1
  BS.skip 2                                   @@ 0x2
  base <- getByteAddr                         @@ 0x4
  binitPc <- getByteAddr                      @@ 0x6
  bdictionaryLoc <- getByteAddr               @@ 0x8
  bobjectTableLoc <- getByteAddr              @@ 0xA
  bglobalVarLoc <- getByteAddr                @@ 0xC
  bbaseStaticAddr <- getByteAddr              @@ 0xE
  bflags2 <- BS.getWord8                      @@ 0x10
  BS.skip 7
  babbrevLoc <- getByteAddr                   @@ 0x18
  blength <- getWord16                        @@ 0x1A
  bchecksum <- getWord16                      @@ 0x1C

  return $ Header v bflags1 base binitPc bdictionaryLoc
    bobjectTableLoc bglobalVarLoc bbaseStaticAddr bflags2 babbrevLoc
    blength bchecksum

readHeaderB :: BR.BinReader Header
readHeaderB = do
  v <- Version . fromIntegral <$> BR.getWord8 -- @@ 0x0
  bflags1 <- BR.getWord8                      -- @@ 0x1
  BR.skip 2                                   -- @@ 0x2
  base <- getByteAddrB                         -- @@ 0x4
  binitPc <- getByteAddrB                      -- @@ 0x6
  bdictionaryLoc <- getByteAddrB               -- @@ 0x8
  bobjectTableLoc <- getByteAddrB              -- @@ 0xA
  bglobalVarLoc <- getByteAddrB                -- @@ 0xC
  bbaseStaticAddr <- getByteAddrB              -- @@ 0xE
  bflags2 <- BR.getWord8                      -- @@ 0x10
  BR.skip 7
  babbrevLoc <- getByteAddrB                   -- @@ 0x18
  blength <- BR.getWord16                        -- @@ 0x1A
  bchecksum <- BR.getWord16                      -- @@ 0x1C

  return $ Header v bflags1 base binitPc bdictionaryLoc
    bobjectTableLoc bglobalVarLoc bbaseStaticAddr bflags2 babbrevLoc
    blength bchecksum

readStory :: B.ByteString -> Either String Story
readStory bstr = do
  let eheader = (fst . BS.runGet readHeader) bstr
  vheader <- eheader
  let staticOffset = toInt . baseStaticAddr $ vheader
  let (dyn,stat) = B.splitAt staticOffset bstr
  return $ Story vheader dyn stat -- (B.take staticOffset (B.singleton 0)

v3OrLower :: Version -> Bool
v3OrLower (Version v) = v <= 3
