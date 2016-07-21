module BinReader where
    
import Data.Word
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Bits

type BinReader a = StateT (B.ByteString,Int) (Either String) a


run :: B.ByteString -> BinReader a -> Either String a
run bs br = do
    (x, _st) <- runStateT br (bs,0) 
    return x

peekWord8 :: BinReader Word8
peekWord8 = do
    (bs, pos) <- get
    let w = B.index bs pos
    return w

getWord8 :: BinReader Word8
getWord8 = do
    (bs, pos) <- get
    let w = B.index bs pos
    put (bs, pos + 1)
    return w

getWord16 :: BinReader Word16
getWord16 = do
    w1 <- getWord8
    w2 <- getWord8
    let w = shift (fromIntegral w1) 8 .|. fromIntegral w2
    return w

getBit :: Int -> BinReader Bool
getBit i = do
    w <- peekWord8
    return $ testBit w i
    
getBits :: Int -> Int -> BinReader Word8
getBits offset n = do
    w <- peekWord8
    return (w `shift` (-offset) .&. (1 `shift` n - 1))
    -- return $ (w `shift` (-offset)) `shift` n `shift` (-n)

skip :: Int -> BinReader ()
skip i = do
    (bs,pos) <- get
    put (bs, pos+i)

setAt :: Int -> BinReader ()
setAt x = do
    (bs,_) <- get
    put (bs, x)

getPos :: BinReader Int
getPos = snd <$> get
