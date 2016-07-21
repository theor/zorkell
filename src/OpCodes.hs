{-# LANGUAGE BinaryLiterals #-}

module OpCodes where

import BinUtils
import Types
import Header
import qualified Data.Vector as V
import Data.Word
import Data.Either
import Control.Monad
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
import qualified Data.Binary.Strict.BitGet as BG

data BranchForm = BrTrue | BrFalse | BrAddress ByteAddr
  deriving (Show, Eq)

data OpcodeForm = LongForm | ShortForm | ExtendedForm | VariableForm deriving (Show, Eq)
data OperandCount = Op0Count | Op1Count | Op2Count | VarCount deriving (Show, Eq)
data OperandType =
    LargeOpandType
  | SmallOpandType
  | VarOpandType
  | OmittedType
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
      0b11 -> return VariableForm
      0b10 -> return ShortForm
      -- Extended
      _ -> return LongForm

readOpandCount :: OpcodeForm -> B.ByteString -> BS.Get OperandCount
readOpandCount form  w =
  either fail return $ BG.runBitGet w $ do
    BG.skip 2
    b4 <- BG.getBit
    b5 <- BG.getBit
    case form of
      LongForm -> return Op2Count
      ShortForm ->
        case (b4,b5) of
          (True,True) -> return Op0Count
          _ -> return Op1Count
      VariableForm -> if b5 then return VarCount else return Op2Count
      ExtendedForm -> return VarCount



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
fetchBytecode w ExtendedForm _ = (extBytecodes V.!) . fromIntegral <$> BS.getWord8
fetchBytecode w _ Op0Count = fetch w 4 4 zeroOperandBytecodes
fetchBytecode w _ Op1Count = fetch w 4 4 oneOperandBytecodes
fetchBytecode w _ Op2Count = fetch w 3 5 twoOperandBytecodes
fetchBytecode w _ VarCount = fetch w 3 5 varOperandBytecodes

readOpBytecode :: BS.Get (OpcodeForm, OperandCount, Bytecode, [OperandType])
readOpBytecode = do
  w <- BS.getByteString 1
  (f,c) <- readOpFormCount w
  bc <- fetchBytecode w f c
  opandTypes <- readOpandType w f c bc
--   traceShowM (f,c, bc, opandTypes)
  return (f,c, bc, opandTypes)
      --  (_, _) -> return ILLEGAL

readOpandType :: B.ByteString -> OpcodeForm -> OperandCount -> Bytecode -> BS.Get [OperandType]
readOpandType w f c op =
        case (f,c) of
            (_, Op0Count) -> runBitGet $ return []
            (_, Op1Count) -> runBitGet $ do
                            BG.skip 2
                            n <- BG.getAsWord8 2
                            traceShowM ("opandType",n)
                            return [decodeOpandTypes n]
            (LongForm, Op2Count) -> runBitGet $ do
                            BG.skip 1
                            let f x = if x then VarOpandType else SmallOpandType 
                            b1 <- f <$> BG.getBit
                            b2 <- f<$> BG.getBit
                            return [b1, b2]
            (VariableForm, _) | op == VAR_236 || op == VAR_250 -> do
                w2 <- BS.getByteString 2
                either fail return $ BG.runBitGet w2 $ readVar 0
            _ -> do w2 <- BS.getByteString 1
                    either fail return $ BG.runBitGet w2 $ readVar 0
    where
        runBitGet g = either fail return $ BG.runBitGet w g
        readVar :: Int -> BG.BitGet [OperandType]
        readVar i = if i == 4
                    then return []
                    else do
                           n <- BG.getAsWord8 2
                           rest <- readVar (i+1)
                           return $ case decodeOpandTypes n of
                               OmittedType -> []
                               x -> x : rest

getTypeLength :: OpcodeForm -> Bytecode -> Int
getTypeLength form opcode =
    case (form, opcode) of
      (VariableForm, VAR_236) -> 2
      (VariableForm, VAR_250) -> 2
      (VariableForm, _) -> 1
      _ -> 0
    
decodeOpandTypes :: (Eq a, Num a) => a -> OperandType
decodeOpandTypes 0 = LargeOpandType
decodeOpandTypes 1 = SmallOpandType
decodeOpandTypes 2 = VarOpandType
decodeOpandTypes _ = OmittedType

data Var = Local Int | Global Int | Stack deriving(Show, Eq)
data Operand = Large Word16 | Small Word8 | Variable Var deriving(Show, Eq)

data OpN =
      Op0 Bytecode
    | Op1 (Bytecode, Operand)
    | Op2 (Bytecode, Operand, Operand)
    deriving(Show, Eq)
opnOp :: OpN -> Bytecode
opnOp (Op0 b) = b
opnOp (Op1(b, _)) = b
opnOp (Op2(b, _, _)) = b

-- data Store = Stack
newtype Op = Op (OpN, Maybe Branch, Maybe Var)
    deriving(Show, Eq)

readOperand :: OperandType -> BS.Get Operand
readOperand SmallOpandType = Small <$> BS.getWord8
readOperand LargeOpandType = Large <$> BS.getWord16be
readOperand VarOpandType = do
    w <- BS.getWord8
    return . Variable $ case w of
        0x00 -> Stack
        x | x <= 0x0f -> Local . fromIntegral $ x
        x -> Global . fromIntegral $ x
readOperand _ = fail "READ OPERAND FAIL"

readOperands :: Traversable t => t OperandType -> BS.Get (t Operand)
readOperands = mapM readOperand

readOpN :: Version -> BS.Get OpN
readOpN ver = do
    (f,c,b,t) <- readOpBytecode
    o <- readOperands t
    case (Prelude.length t, o) of
        (0, []) -> return $ Op0 b
        (1, [o1]) -> return $ Op1(b, o1)
        (2, [o1,o2]) -> return $ Op2(b, o1, o2)
        _ -> fail "WEIRD"

readOp :: Version -> BS.Get Op
readOp ver = do
    opn <- readOpN ver
    let bc = opnOp opn
    br <- readBranchMaybe bc ver
    return $ Op(opn, br, Nothing)

-- Branching

hasBranch :: Bytecode -> Version -> Bool
hasBranch opcode ver =
  case opcode of
    OP0_181 -> Header.v3OrLower ver -- "save" branches in v3, stores in v4
    OP0_182 -> Header.v3OrLower ver -- "restore" branches in v3, stores in v4
    x | x == OP2_1   || x == OP2_2   || x == OP2_3   || x == OP2_4   || x == OP2_5   || x == OP2_6 || x == OP2_7 || x == OP2_10 ||
        x == OP1_128 || x == OP1_129 || x == OP1_130 || x == OP0_189 || x == OP0_191 ||
        x == VAR_247 || x == VAR_255 ||
        x == EXT_6   || x == EXT_14  || x == EXT_24  || x == EXT_27 -> True
    _ -> False

type Branch = (Bool, BranchForm)
readBranch :: BS.Get Branch
readBranch = do
    w <- BS.getByteString 1
    either fail return $ BG.runBitGet w g
    where
        g = do sense <- BG.getBit -- b7
               short <- BG.getBit -- b6
               offset <- if short
               then BG.getAsWord16 6
               else BG.getAsWord16 14
               case offset of
                 0 -> return (sense, BrFalse)
                 1 -> return (sense, BrTrue)
                 _ -> return (sense, BrAddress . ByteAddr $ offset)
            --    fail "ASDASD"



readBranchMaybe :: Bytecode -> Version -> BS.Get (Maybe Branch)
readBranchMaybe b v =
    if hasBranch b v
    then do b <- readBranch
            return $ Just b
    else return Nothing