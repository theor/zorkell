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
  -- it "asdasd" $
  --   getAbbrEntryIndex 2 (Abbr 9) `shouldBe` 73
  -- it "asdasd" $
  --   getAbbrEntryIndex 0 (Abbr 0) `shouldBe` 0

  -- it "Abbrev 0-0 should be 'the '" $ do
  --   s <- readStory <$> minizork ()
  --   ev (run s $ getAbbr 0 (Abbr 0)) (`shouldBe` "the ")
  --
  -- it "Abbrev 2-9 should be 'an '" $ do
  --   s <- readStory <$> minizork ()
  --   ev (run s $ getAbbr 2 (Abbr 9)) (`shouldBe` "an ")
  --
  -- it "Abbrev 0-1 should be 'The '" $ do
  --   s <- readStory <$> minizork ()
  --   ev (run s $ getAbbr 0 (Abbr 1)) (`shouldBe` "The ")
  --
  -- it "Abbrev 1-0 should be 'Room'" $ do
  --   s <- readStory <$> minizork ()
  --   ev (run s $ getAbbr 1 (Abbr 0)) (`shouldBe` "Room")
  --
  -- it "Abbrev 2-28 should be 'staircase '" $ do
  --   s <- readStory <$> minizork ()
  --   ev (run s $ getAbbr 2 (Abbr 28)) (`shouldBe` "staircase ")

  -- it "should unpack zstring el" $
  --   case BG.runBitGet (B.pack [0x65, 0xaa]) ZString.decode of
  --     Left a -> expetctationFailure a
  --     Right a -> a `shouldBe` (False, [0x19 :: Word8, 0x0d :: Word8, 0x0a :: Word8])
  --
  -- it "should unpack zstring" $
  --   case BG.runBitGet (B.pack [0b11010011,0b00100001]) ZString.decode of
  --     Left a -> expectationFailure a
  --     Right a -> a `shouldBe` (True, [0b10100 :: Word8, 0b11001 :: Word8, 0b1:: Word8])
  --
  -- it "should decode zstring 'the'" $
  --   ZString.decodeString (B.pack [0x65, 0xaa]) `shouldBe` "the"
  --
  -- it "should decode zstring 'the '" $
  --   ZString.decodeString (B.pack [0x65, 0xaa, 0x80, 0xa5]) `shouldBe` "the "
  it "should decode zstring 'Egyptian Room'" $
    ZString.decodeString (B.pack [0x11, 0x4c, 0x7a, 0xb9, 0x38, 0x69, 0x88, 0x05]) `shouldBe` "Egyptian Room"
  -- it "should decode zstring 'Egyptian Room'" $
  --   ZString.decodeString (B.pack [0x1d, 0x4e, 0x17, 0x4e, 0x2e, 0x5b, 0x44, 0x0c]) `shouldBe` "an Room"
