{-# LANGUAGE BinaryLiterals #-}

module OpCodesSpec where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG

import Utils
import Header
import Test.Hspec

import OpCodes

runOp :: (B.ByteString -> S.Get a) -> B.ByteString -> Either String a
runOp  f s = fst . S.runGet (f s) $ s

spec = describe "Opcodes" $ do
  it "should decode short form" $
    ev (runOp OpCodes.readForm (B.pack [0b10000000]))
      (`shouldBe` ShortForm)

  it "should decode var form" $
    ev (runOp OpCodes.readForm (B.pack [0b11000000]))
      (`shouldBe` VariableForm)

  it "should read long form; count 2OP; opcode number 5; 2 small operands" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x05, 0x02, 0x00, 0xd4])
      (\x -> do print x
                x `shouldBe` (LongForm,Op2Count,OP2_5,[SmallOpandType,SmallOpandType]))
  it "should read long form; count 2OP; opcode number 5; 2 small operands" $
    ev (fst $ S.runGet (OpCodes.readOpN $ Version 3) (B.pack [0x05, 0x02, 0x00, 0xd4]))
      (\x -> do print x
                x `shouldBe` Op2 (OP2_5, Small 0x2, Small 0 ))
  it "should read long form; count 2OP; opcode number 5; 2 small operands" $
    ev (fst $ S.runGet (OpCodes.readOp $ Version 3) (B.pack [0x05, 0x02, 0x00, 0xd4]))
      (\x -> do print x
                x `shouldBe` Op(Op2 (OP2_5, Small 0x2, Small 0 ), Just (True, BrTrue), Nothing))

  it "should read short form; count 1OP; opcode number 15; operand: Large" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0x8f, 0x01, 0x56]) 
      (\x -> do print x
                x `shouldBe` (ShortForm,Op1Count,OP1_143,[LargeOpandType]))
  it "should read short form; count 1OP; opcode number 15; operand: Large" $
    ev (fst $ S.runGet (OpCodes.readOpN (Version 3)) (B.pack [0x8f, 0x01, 0x56])) 
    -- ev ((fst . S.runGet) (OpCodes.readOp (Version 3)) (B.pack [0x8f, 0x01, 0x56])) 
      (\x -> do print x
                x `shouldBe` Op1 (OP1_143, Large 342))

  it "should read short form; count 0OP; literal string" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0xb2]) 
      (\x -> do print x
                x `shouldBe` (ShortForm,Op0Count,OP0_178,[]))
  it "should read variable form; count 2OP; opcode number 22; operands: Large, Var" $
    ev (fst . S.runGet OpCodes.readOpBytecode $ B.pack [0xd6, 0x2f, 0x03, 0xe8, 0x02, 0x00]) 
      (\x -> do print x
                x `shouldBe` (VariableForm,VarCount,VAR_246,[LargeOpandType, VarOpandType]))