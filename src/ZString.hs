{-|
Description : ZString management
-}
module ZString (
    decodeString
  , decode
  , getAbbrAddress
  , getAbbrEntryIndex
  , getAbbr
  , Abbr(..)
) where

import BinUtils
import Types
import Header
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Control.Exception.Base
import Control.Monad.State.Lazy
import Control.Arrow
import Debug.Trace

newtype Abbr = Abbr Integer
  deriving (Show)
--
-- newtype WordZstringAddress = WordZstring Integer
-- newtype ZstringAddress = Zstring Integer
--
-- getAbbrWordAddress ::  Abbr -> WordZstringAddress
-- getAbbrWordAddress (Abbr a) = WordZstring 0

getAbbrEntryIndex :: Int -> Abbr -> Int
getAbbrEntryIndex z (Abbr a) = 32*z + fromIntegral a

getAbbrOffset :: Int -> Abbr -> ByteAddr
getAbbrOffset z a =
  ByteAddr . fromIntegral $ getAbbrEntryIndex z a

getAbbrAddress :: Int -> Abbr -> StoryReader ByteAddr
getAbbrAddress z a = do
  h <- getHeader
  let abbrStart = Header.abbrevLoc h
  traceShowM ("abbrevLoc", abbrStart, "z,a", (z,a))
  let abbrAddr = getAbbrOffset z a
  setAtAddr (traceShowId $ abbrStart + abbrAddr*2 )
  wordAddr <- exec S.getWord16be
  traceShowM("word addr", wordAddr)
  return $ ByteAddr . (*2) . fromIntegral $ wordAddr

getAbbr :: Int -> Abbr -> StoryReader String
getAbbr z a = do
  strAddr <- getAbbrAddress z a
  traceShowM ("Abbr addr", strAddr, z, a)
  setAtAddr strAddr
  str <- exec . S.getByteString $ 12
  return $ ZString.decodeString str

type Z3Pack = (Bool, [Word8])

alphabetTable :: [String]
alphabetTable = [ " ?????abcdefghijklmnopqrstuvwxyz",
                  " ?????ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                  " ??????\n0123456789.,!?_#'\"/\\-:()" ]

-- |extract a tuple (isEnd,[char1,char2,char3]) from the ByteString
decode :: BG.BitGet Z3Pack
decode = do
  end <- BG.getBit
  c1 <- BG.getAsWord8 5
  c2 <- BG.getAsWord8 5
  c3 <- BG.getAsWord8 5
  traceShowM (end, c1, c2, c3)
  return (end,[c1,c2,c3])

at :: Int -> Word8 -> Char
at a = (Prelude.!!) (alphabetTable !! a) . (\x -> fromIntegral x :: Int)

data DecodeState = Alphabet Int | Abbrev Int | Leading | Trailing Word8 deriving (Show)

stateMachine :: DecodeState -> Word8 -> (String, DecodeState)
stateMachine decState x = runState comp decState
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

          (Alphabet a, i) -> do { put (Alphabet 0); return [at a i] }

          (Abbrev a, x) -> do { put (Alphabet 0); return $ "<" ++ show a ++ "," ++ show x ++ ">" }
    }



decodeString_ :: DecodeState -> B.ByteString -> String
decodeString_ decState s =
  traceShow ("decodeString_", B.unpack s) $
  case BG.runBitGet s decode of
    Left e -> trace ("END OF STRING" ++ e) []
    Right (end,c) ->
      let f (str, st) x = first (str ++) $ stateMachine st x
          (string, finalState) = foldl f ("",decState) c in
        traceShow (string, finalState, end) $
        string ++ if end then "" else decodeString_ finalState (B.drop 2 s)

-- |decodes a string from a ByteString, starting at offset 0
decodeString :: B.ByteString -> String
decodeString = decodeString_ (Alphabet 0)
