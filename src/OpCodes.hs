{-# LANGUAGE BinaryLiterals #-}

module OpCodes where

import BinUtils
import qualified Data.Vector as V
import Data.Word
import Data.Either
import Control.Monad
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
import qualified Data.Binary.Strict.BitGet as BG

data OpcodeForm = Long | Short | Extended | Variable deriving (Show, Eq)
data OperandCount = Op0 | Op1 | Op2 | Var deriving (Show, Eq)
data OperandType =
    LargeOperand
  | SmallOperand
  | VariableOperand
  | Omitted
  deriving (Show, Eq)
data Bytecode =
              OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7
  | OP2_8   | OP2_9   | OP2_10  | OP2_11  | OP2_12  | OP2_13  | OP2_14  | OP2_15
  | OP2_16  | OP2_17  | OP2_18  | OP2_19  | OP2_20  | OP2_21  | OP2_22  | OP2_23
  | OP2_24  | OP2_25  | OP2_26  | OP2_27  | OP2_28
  | OP1_128 | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_133 | OP1_134 | OP1_135
  | OP1_136 | OP1_137 | OP1_138 | OP1_139 | OP1_140 | OP1_141 | OP1_142 | OP1_143
  | OP0_176 | OP0_177 | OP0_178 | OP0_179 | OP0_180 | OP0_181 | OP0_182 | OP0_183
  | OP0_184 | OP0_185 | OP0_186 | OP0_187 | OP0_188 | OP0_189 | OP0_190 | OP0_191
  | VAR_224 | VAR_225 | VAR_226 | VAR_227 | VAR_228 | VAR_229 | VAR_230 | VAR_231
  | VAR_232 | VAR_233 | VAR_234 | VAR_235 | VAR_236 | VAR_237 | VAR_238 | VAR_239
  | VAR_240 | VAR_241 | VAR_242 | VAR_243 | VAR_244 | VAR_245 | VAR_246 | VAR_247
  | VAR_248 | VAR_249 | VAR_250 | VAR_251 | VAR_252 | VAR_253 | VAR_254 | VAR_255
  | EXT_0   | EXT_1   | EXT_2   | EXT_3   | EXT_4   | EXT_5   | EXT_6   | EXT_7
  | EXT_8   | EXT_9   | EXT_10  | EXT_11  | EXT_12  | EXT_13  | EXT_14
  | EXT_16  | EXT_17  | EXT_18  | EXT_19  | EXT_20  | EXT_21  | EXT_22  | EXT_23
  | EXT_24  | EXT_25  | EXT_26  | EXT_27  | EXT_28  | EXT_29
  | ILLEGAL
  deriving (Show, Eq)

  -- The tables which follow are maps from the opcode identification number
  --   to the opcode type; the exact order matters.

oneOperandBytecodes :: V.Vector Bytecode
oneOperandBytecodes = V.fromList [
    OP1_128, OP1_129, OP1_130, OP1_131, OP1_132, OP1_133, OP1_134, OP1_135,
    OP1_136, OP1_137, OP1_138, OP1_139, OP1_140, OP1_141, OP1_142, OP1_143  ]

zeroOperandBytecodes :: V.Vector Bytecode
zeroOperandBytecodes = V.fromList [
    OP0_176, OP0_177, OP0_178, OP0_179, OP0_180, OP0_181, OP0_182, OP0_183,
    OP0_184, OP0_185, OP0_186, OP0_187, OP0_188, OP0_189, OP0_190, OP0_191  ]

twoOperandBytecodes :: V.Vector Bytecode
twoOperandBytecodes = V.fromList [
    ILLEGAL, OP2_1,  OP2_2,  OP2_3,  OP2_4,  OP2_5,   OP2_6,   OP2_7,
    OP2_8,   OP2_9,  OP2_10, OP2_11, OP2_12, OP2_13,  OP2_14,  OP2_15,
    OP2_16,  OP2_17, OP2_18, OP2_19, OP2_20, OP2_21,  OP2_22,  OP2_23,
    OP2_24,  OP2_25, OP2_26, OP2_27, OP2_28, ILLEGAL, ILLEGAL, ILLEGAL ]

varOperandBytecodes :: V.Vector Bytecode
varOperandBytecodes = V.fromList [
    VAR_224, VAR_225, VAR_226, VAR_227, VAR_228, VAR_229, VAR_230, VAR_231,
    VAR_232, VAR_233, VAR_234, VAR_235, VAR_236, VAR_237, VAR_238, VAR_239,
    VAR_240, VAR_241, VAR_242, VAR_243, VAR_244, VAR_245, VAR_246, VAR_247,
    VAR_248, VAR_249, VAR_250, VAR_251, VAR_252, VAR_253, VAR_254, VAR_255 ]

extBytecodes :: V.Vector Bytecode
extBytecodes = V.fromList [
    EXT_0,   EXT_1,   EXT_2,   EXT_3,   EXT_4,   EXT_5,   EXT_6,   EXT_7,
    EXT_8,   EXT_9,   EXT_10,  EXT_11,  EXT_12,  EXT_13,  EXT_14,  ILLEGAL,
    EXT_16,  EXT_17,  EXT_18,  EXT_19,  EXT_20,  EXT_21,  EXT_22,  EXT_23,
    EXT_24,  EXT_25,  EXT_26,  EXT_27,  EXT_28,  EXT_29,  ILLEGAL, ILLEGAL ]

readForm :: B.ByteString -> BS.Get OpcodeForm
readForm w =
  either fail return $ BG.runBitGet w $ do
    b <- BG.getAsWord8 2
    case b of
      0b11 -> return Variable
      0b10 -> return Short
      -- Extended
      _ -> return Long

readOpandCount :: OpcodeForm -> B.ByteString -> BS.Get OperandCount
readOpandCount form  w =
  either fail return $ BG.runBitGet w $ do
    BG.skip 2
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



readOpFormCount :: B.ByteString -> BS.Get (OpcodeForm, OperandCount)
readOpFormCount w = do
    form <- readForm w
    opandCount <- readOpandCount form w
    return (form,opandCount)

-- fetch :: B.ByteString -> Int -> Int -> V.Vector r -> BS.Get r

fetch :: B.ByteString -> Int -> Int -> V.Vector r -> BS.Get r
fetch s skip count array =
  either fail return $ BG.runBitGet s $ do
    BG.skip skip
    idx <- fromIntegral <$> BG.getAsWord8 count
    return $ array V.! idx

fetchBytecode :: B.ByteString -> OpcodeForm -> OperandCount -> BS.Get Bytecode
fetchBytecode w Extended _ = (extBytecodes V.!) . fromIntegral <$> BS.getWord8
fetchBytecode w _ Op0 = fetch w 4 4 zeroOperandBytecodes
fetchBytecode w _ Op1 = fetch w 4 4 oneOperandBytecodes
fetchBytecode w _ Op2 = fetch w 3 5 twoOperandBytecodes
fetchBytecode w _ Var = fetch w 3 5 varOperandBytecodes

readOpBytecode :: BS.Get (OpcodeForm, OperandCount, Bytecode, [OperandType])
readOpBytecode = do
  w <- BS.getByteString 1
  (f,c) <- readOpFormCount w
  bc <- fetchBytecode w f c
  opandTypes <- readOpandType w f c
--   traceShowM (f,c, bc, opandTypes)
  return (f,c, bc, opandTypes)
      --  (_, _) -> return ILLEGAL

readOpandType :: B.ByteString -> OpcodeForm -> OperandCount -> BS.Get [OperandType]
readOpandType w f c =
        case (f,c) of
            (_, Op0) -> runBitGet $ return []
            (_, Op1) -> runBitGet $ do
                            BG.skip 2
                            n <- BG.getAsWord8 2
                            traceShowM ("opandType",n)
                            return [decodeOpandTypes n]
            (Long, Op2) -> runBitGet $ do
                            BG.skip 1
                            let f x = if x then VariableOperand else SmallOperand 
                            b1 <- f <$> BG.getBit
                            b2 <- f<$> BG.getBit
                            return [b1, b2]
            -- (Variable, _) |
            _ -> do w2 <- B.append w <$> BS.getByteString 1
                    either fail return $ BG.runBitGet w2 $ do
                         BG.skip 8
                         readVar 0
    where
        runBitGet g = either fail return $ BG.runBitGet w g
        readVar :: Int -> BG.BitGet [OperandType]
        readVar i = if i == 4
                    then return []
                    else do
                           n <- BG.getAsWord8 2
                           rest <- readVar (i+1)
                           return $ case decodeOpandTypes n of
                               Omitted -> []
                               x -> x : rest
    -- form <- readForm w
    -- opandCount <- readOpandCount form w
    -- return (form,opandCount)
    
decodeOpandTypes :: (Eq a, Num a) => a -> OperandType
decodeOpandTypes 0 = LargeOperand
decodeOpandTypes 1 = SmallOperand
decodeOpandTypes 2 = VariableOperand
decodeOpandTypes _ = Omitted

data OpCode = Ld
