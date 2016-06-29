{-# LANGUAGE BinaryLiterals #-}

module OpCodes where

import BinUtils
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
import qualified Data.Binary.Strict.BitGet as BG

data OpcodeForm = Long | Short | Extended | Variable deriving (Show, Eq)
data OperandCount = Op0 | Op1 | Op2 | Var deriving (Show, Eq)

readForm :: BG.BitGet OpcodeForm
readForm = do
  b <- BG.getAsWord8 2
  case b of
    0b11 -> return Variable
    0b10 -> return Short
    -- Extended
    _ -> return Long

readOpandCount :: OpcodeForm -> BG.BitGet OperandCount
readOpandCount form = do
  BG.skip 4
  b4 <- BG.getBit
  b5 <- BG.getBit
  case form of
    Long -> return Op2
    Short ->
      case (b4,b5) of
        (True,True) -> return Op0
        _ -> return Op1
    Variable -> if b5 then return Var else return Op2
    Extended -> return Var

  -- return Op0

readOp = do
  byte <- BS.getByteString 1 @@ 0
  return $ do
    form <- BG.runBitGet byte readForm
    opandCount <- BG.runBitGet byte $ readOpandCount form
    return (form,opandCount)
  -- return 1

data OpCode = Ld
