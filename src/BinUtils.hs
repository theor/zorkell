module BinUtils where

  import Data.Word
  import Text.Printf

  import Types
  import qualified Data.Binary.Strict.Get as BS
  import qualified BinReader as BR

  getWord16 :: BS.Get Word16
  getWord16 = BS.getWord16be

  getByteAddr :: BS.Get ByteAddr
  getByteAddr = ByteAddr <$> getWord16

  getByteAddrB :: BR.BinReader ByteAddr
  getByteAddrB = ByteAddr <$> BR.getWord16

  toInt :: ByteAddr -> Int
  toInt (ByteAddr a) = fromIntegral a :: Int

  -- class (Monad m) => ReadCount m where
  --   bytesRead :: m Int
  -- instance ReadCount BS.Get where
  --   bytesRead = BS.bytesRead
  -- instance ReadCount BG.BitGet where
  --   bytesRead = BG.r

  -- (@@) :: BS.Get b -> Int -> BS.Get b
  (@@) :: BS.Get b -> Int -> BS.Get b
  f @@ i = do
    bytesRead <- BS.bytesRead
    if bytesRead == i then f else fail $ printf "mismatch, should be at byte 0x%02x, is at 0x%02x" i bytesRead
    
  (@@@) :: BS.Get b -> ByteAddr -> BS.Get b
  f @@@ (ByteAddr a) = f @@ (fromIntegral a :: Int)
