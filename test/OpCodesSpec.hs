{-# LANGUAGE BinaryLiterals #-}

module OpCodesSpec where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG

import Utils
import Test.Hspec

import OpCodes

run  f s = fst . S.runGet (f s) $ s
spec = describe "Opcodes" $ do
  it "should decode short form" $ do
    ev (run OpCodes.readForm (B.pack [0b10000000]))
      (`shouldBe` Short)

  it "should decode var form" $ do
    ev (run OpCodes.readForm (B.pack [0b11000000]))
      (`shouldBe` Variable)

  it "should read opcode number" $ do
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x05, 0x02])
      (print . (,) "long form; count 2OP")
  it "should read opcode number" $ do
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x8f]) 
      (print . (,) "long form; count 2OP")
