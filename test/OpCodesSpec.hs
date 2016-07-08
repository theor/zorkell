{-# LANGUAGE BinaryLiterals #-}

module OpCodesSpec where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG

import Utils
import Test.Hspec

import OpCodes

run :: (B.ByteString -> S.Get a) -> B.ByteString -> Either String a
run  f s = fst . S.runGet (f s) $ s

spec = describe "Opcodes" $ do
  it "should decode short form" $
    ev (run OpCodes.readForm (B.pack [0b10000000]))
      (`shouldBe` Short)

  it "should decode var form" $
    ev (run OpCodes.readForm (B.pack [0b11000000]))
      (`shouldBe` Variable)

  it "should read long form; count 2OP; opcode number 5; 2 small operands" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x05, 0x02])
      (\x -> do print x
                x `shouldBe` (Long,Op2,OP2_5,[SmallOperand,SmallOperand]))
  it "should read short form; count 1OP; opcode number 15; operand: Large" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x8f]) 
      (\x -> do print x
                x `shouldBe` (Short,Op1,OP1_143,[LargeOperand]))

  it "should read short form; count 0OP; literal string" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0xb2]) 
      (\x -> do print x
                x `shouldBe` (Short,Op0,OP0_178,[]))
  it "should read variable form; count 2OP; opcode number 22; operands: Large, Var" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0xd6, 0x2f]) 
      (\x -> do print x
                x `shouldBe` (Variable,Var,VAR_246,[LargeOperand, VariableOperand]))