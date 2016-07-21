{-# LANGUAGE BinaryLiterals #-}

module BinReaderSpec where

import Text.Printf
import qualified Data.ByteString as B
-- import qualified Data.Binary.Strict.Get as S

import Test.Hspec
import Utils

import qualified BinReader as BR

bs = B.pack [0x12,0x34,0x56,0x78] -- 00010010 0011 0100 ...
runPack = BR.run . B.pack

pb = printf "%08b\n"

spec :: Spec
spec = describe "BinReader" $ do
  it "tmp" $ print 0

  it "should read 1 Word8" $ do
      let w = BR.run bs $ BR.getWord8
      w `shouldBe` Right 0x12
      print w
      
  it "should read 1 Word16" $ do
      let w = BR.run bs $ BR.getWord16
      w `shouldBe` Right 0x1234
      print w

  it "should read second bit" $ do
      let w = BR.run bs $ BR.getBit 1
      w `shouldBe` Right True
      print w

  it "should get second bit alone" $ do
      let w = BR.run bs $ BR.getBits 1 1
      w `shouldBe` Right 0b1
      print w
      
  it "should get middle bits 001001" $ do
      let w = BR.run bs $ BR.getBits 1 6
      w `shouldBe` Right 0b001001
      either print pb w
  it "should get middle bits 101" $ do
      let w = runPack [0b00101010] $ BR.getBits 1 3
      w `shouldBe` Right 0b101
      print w
  it "should get front bits 0010" $ do
      let w = runPack [0b00101010] $ BR.getBits 4 4
      w `shouldBe` Right 0b0010
      print w

  it "should read 2 Word8" $ do
      let w = BR.run bs $ do w1 <- BR.getWord8
                             w2 <- BR.getWord8
                             return (w1,w2)
      w `shouldBe` Right (0x12, 0x34)
      print w

  it "should skip then read 1 Word8" $ do
      let w = BR.run bs $ do BR.setAt 1
                             BR.getWord8
      w `shouldBe` Right 0x34
      print w