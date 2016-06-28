module ZString where

import qualified Data.Binary.Strict.BitGet as BG
import Data.Either

newtype Abbr = Abbr Integer

newtype WordZstringAddress = WordZstring Integer
newtype ZstringAddress = Zstring Integer

getAbbrWordAddress ::  Abbr -> WordZstringAddress
getAbbrWordAddress (Abbr a) = WordZstring 0

decode = do
  end <- BG.getBit
  c1 <- BG.getAsWord8 5
  c2 <- BG.getAsWord8 5
  c3 <- BG.getAsWord8 5
  return (end,c1,c2,c3)
