module ZString where

import qualified Data.ByteString as B
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.Binary.Strict.Get as S
import Data.Word
import Data.Either

newtype Abbr = Abbr Integer

newtype WordZstringAddress = WordZstring Integer
newtype ZstringAddress = Zstring Integer

getAbbrWordAddress ::  Abbr -> WordZstringAddress
getAbbrWordAddress (Abbr a) = WordZstring 0

type Z3Pack = (Bool, [Word8])

alphabetTable = " ?????abcdefghijklmnopqrstuvwxyz"

decode :: BG.BitGet Z3Pack
decode = do
  end <- BG.getBit
  c1 <- BG.getAsWord8 5
  c2 <- BG.getAsWord8 5
  c3 <- BG.getAsWord8 5
  return (end,[c1,c2,c3])

at = (Prelude.!!) alphabetTable . (\x -> fromIntegral x :: Int)
-- decodeString s :: BG.BitGet [Word8]
decodeString s =
  case BG.runBitGet s decode of
    Left a -> []
    Right (True,c) -> map at c
    Right (False,c) -> map at c ++ decodeString (B.drop 2 s)
