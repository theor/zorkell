{-# LANGUAGE BinaryLiterals #-}

module ZStringSpec where

import Debug.Trace

import BinUtils
import Utils
import Test.Hspec
import qualified ObjectTable as OT
import Data.Either
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG
import Control.Monad
import Control.Monad.Except
import Text.Printf

import Types
import Header
import ZString

spec = describe "Decode Abbrev" $ do
  it "asdasd" $
    getAbbrEntryIndex 2 (Abbr 9) `shouldBe` 73
  it "asdasd" $
    getAbbrEntryIndex 0 (Abbr 0) `shouldBe` 0

  it "Abbrev 0-0 should be 'the '" $ do
    s <- readStory <$> minizork ()
    ev (run s $ getAbbr 0 (Abbr 0)) (`shouldBe` "the ")

  it "Abbrev 2-9 should be 'an '" $ do
    s <- readStory <$> minizork ()
    ev (run s $ getAbbr 2 (Abbr 9)) (`shouldBe` "an ")

  it "Abbrev 0-1 should be 'The '" $ do
    s <- readStory <$> minizork ()
    ev (run s $ getAbbr 0 (Abbr 1)) (`shouldBe` "The ")

  it "Abbrev 1-0 should be 'Room'" $ do
    s <- readStory <$> minizork ()
    ev (run s $ getAbbr 1 (Abbr 0)) (`shouldBe` "Room")

  it "Abbrev 2-28 should be 'staircase '" $ do
    s <- readStory <$> minizork ()
    ev (run s $ getAbbr 2 (Abbr 28)) (`shouldBe` "staircase ")

  it "should unpack zstring el" $
    case BG.runBitGet (B.pack [0x65, 0xaa]) ZString.decode of
      Left a -> expectationFailure a
      Right a -> a `shouldBe` (False, [0x19 :: Word8, 0x0d :: Word8, 0x0a :: Word8])

  it "should unpack zstring" $ do
    case BG.runBitGet (B.pack [0b11010011,0b00100001]) ZString.decode of
      -- Left a -> expectationFailure a
      Right a -> a `shouldBe` (True, [0b10100 :: Word8, 0b11001 :: Word8, 0b1:: Word8])
 
  it "should decode zstring 'Egyptian Room'" $ do
    s <- readStory <$> minizork ()
    ev (run s $ do setAt 0x135c
                   ZString.decodeString)
      (`shouldBe` "Egyptian Room")

  it "should decode long 'Floor Control...' string" $ do
        s <- readStory <$> minizork ()
        ev (run s $ do setAt 0xb106
                       ZString.decodeString)
          (`shouldBe` "\"Flood Control Dam #3 was constructed in 783 GUE with a"
          ++ " grant of 37 million zorkmids from Lord Dimwit Flathead the "
          ++ "Excessive. This impressive structure is composed of 370,000 "
          ++ "cubic feet of concrete, is 256 feet tall and 193 feet wide.\n\n"
          ++ "The construction of FCD#3 took 112 days from ground breaking to "
          ++ "dedication. It required a work force of 384 slaves, 34 slave "
          ++ "drivers, and 12 engineers, 2345 bureaucrats, and nearly one "
          ++ "million dead trees.\n\nAs you start your tour, notice the more "
          ++ "interesting features of FCD#3. On your right...")



testDecode expected bs = do
  es <- readStory <$> minizork ()
  let s = do s <- es
             return $ s { dynMem = B.pack bs }
  ev (run s ZString.decodeString) (`shouldBe` expected)
