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
  -- traceShowM ("abbrevLoc", abbrStart, "z,a", (z,a))
  let abbrAddr = getAbbrOffset z a
  setAtAddr (abbrStart + abbrAddr*2 )
  wordAddr <- exec S.getWord16be
  -- traceShowM("word addr", wordAddr)
  return $ ByteAddr . (*2) . fromIntegral $ wordAddr

getAbbr :: Int -> Abbr -> StoryReader String
getAbbr z a = do
  strAddr <- getAbbrAddress z a
  -- traceShowM ("Abbr addr", strAddr, z, a)
  setAtAddr strAddr
  ZString.decodeString

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
  -- traceShowM (end, c1, c2, c3)
  return (end,[c1,c2,c3])

at :: Int -> Word8 -> Char
at a = (Prelude.!!) (alphabetTable !! a) . (\x -> fromIntegral x :: Int)

data DecodeState = Alphabet Int | Abbrev Int | Leading | Trailing Word8 deriving (Show)

stateMachine :: DecodeState -> Word8 -> StoryReader (String, DecodeState)
-- stateMachine decState x --) = comp decState
stateMachine  = comp
  where
    -- comp :: StateT DecodeState Data.Functor.Identity.Identity [Char]
    comp s x = --do {
        -- s <- get;
        case (s,x) of
          (Alphabet _, 1) ->  return ("", Abbrev 0)
          (Alphabet _, 2) ->  return ("", Abbrev 1)
          (Alphabet _, 3) ->  return ("", Abbrev 2)

          (Alphabet _, 4) ->  return ("", Alphabet 1)
          (Alphabet _, 5) ->  return ("", Alphabet 2)

          (Alphabet 2, 6) -> return ("", Leading)

          (Leading, _) -> return ("", Trailing x)
          (Trailing high, _) -> return ([toEnum $ fromIntegral high * 32 + fromIntegral x], Alphabet 0)

          (Alphabet a, i) -> return ([at a i], Alphabet 0)

          (Abbrev a, x) -> do
            -- put (Alphabet 0)
            pos <- isAt
            abbr <- getAbbr a (Abbr . fromIntegral $ x)
            setAt pos
            return (abbr, Alphabet 0)
            -- return $ ("<" ++ show a ++ "," ++ show x ++ ">", Alphabet 0)
    -- }



decodeString_ :: DecodeState -> StoryReader String
decodeString_ decState = do
  bword <- exec $ S.getByteString 2

  -- traceShowM ("decodeString_", B.unpack bword)
  case BG.runBitGet bword decode of
    Left e -> return [] -- $ trace ("END OF STRING" ++ e) []
    Right (end,c) ->
      do
        (string, finalState) <- foldM f ("",decState) c
        let mNext = if end then return "" else decodeString_ finalState
        -- traceShowM (string, finalState, end)
        next <- mNext
        return $ string ++ next
  where
    f (str, st) x = first (str ++) <$> stateMachine st x

-- |decodes a string from a ByteString, starting at offset 0
-- decodeStringD :: B.ByteString -> String
-- decodeStringD = decodeString_ (Alphabet 0)

decodeString :: StoryReader String
decodeString = decodeString_ (Alphabet 0)
