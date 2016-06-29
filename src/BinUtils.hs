module BinUtils where

  import Text.Printf

  import Types
  import qualified Data.Binary.Strict.Get as BS
  import qualified Data.Binary.Strict.BitGet as BG

  getWord16 = BS.getWord16be
  getByteAddr = ByteAddr <$> getWord16

  toInt (ByteAddr a) = fromIntegral a :: Int

  -- class (Monad m) => ReadCount m where
  --   bytesRead :: m Int
  -- instance ReadCount BS.Get where
  --   bytesRead = BS.bytesRead
  -- instance ReadCount BG.BitGet where
  --   bytesRead = BG.r

  f @@ i = do
    bytesRead <- BS.bytesRead
    if bytesRead == i then f else fail $ printf "mismatch, should be at byte 0x%02x, is at 0x%02x" i bytesRead
  f @@@ (ByteAddr a) = f @@ (fromIntegral a :: Int)
