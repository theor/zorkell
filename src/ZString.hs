module ZString (decodeString, decode) where

import qualified Data.ByteString as B
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.Binary.Strict.Get as S
import Data.Word
import Data.Either
import Control.Monad.State.Lazy
import Control.Arrow

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

data DecodeState = Alphabet Int | Abbrev Int | Leading | Trailing Word8
-- dec :: Word8 -> (Maybe Char, DecodeState)
dec :: DecodeState -> Word8 -> (String, DecodeState)
dec state x = runState comp state
  where
    comp = do {
        s <- get;
        case (s,x) of
          (Alphabet _, 1) -> do { put (Abbrev 0); return "" }
          (Alphabet _, 2) -> do { put (Abbrev 1); return "" }
          (Alphabet _, 3) -> do { put (Abbrev 2); return "" }

          (Alphabet _, 4) -> do { put (Alphabet 1); return "" }
          (Alphabet _, 5) -> do { put (Alphabet 2); return "" }

          (Alphabet 2, 6) -> do { put Leading; return "" }

          (Leading, _) -> do { put (Trailing x); return "" }
          (Trailing high, _) -> return [toEnum $ fromIntegral high * 32 + fromIntegral x]

          (Alphabet a, x) -> do { put (Alphabet 0); return [at x] }
        -- return $ at x
    }



decodeString_ :: DecodeState -> B.ByteString -> String
decodeString_ state s =
  case BG.runBitGet s decode of
    Left a -> []
    Right (end,c) ->
      let f (str, st) x = first (str ++) $ dec st x
          (string, finalState) = foldl f ("",state) c in
        string ++ if end then "" else decodeString_ finalState (B.drop 2 s)
        -- foldl at c -- ++ (if end then "" else decodeString_ (B.drop 2 s))


decodeString :: B.ByteString -> String
decodeString = decodeString_ (Alphabet 0)
