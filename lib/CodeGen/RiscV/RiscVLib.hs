{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeGen.RiscV.RiscVLib
  ( compileT,
    compile,
    mainFunction,
    function,
    globalVar,
    externFunction,
    AsmBuilderT,
    AsmBuilder,
    Operand,
    CodeLine,
    immediate,
    and,
    or,
    add,
    sub,
    mul,
    neg,
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
    call,
    ite,
    storeToLabeledAddr,
  )
where

import Control.Monad (replicateM, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..), execStateT, get, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Txt
import Prelude hiding (and, or)

-- | Compile the code
compileT :: (Monad m) => AsmBuilderT m a -> m [CodeLine]
compileT m = concatCode <$> execAsmBuilderT m
  where
    execAsmBuilderT :: (Monad m) => AsmBuilderT m a -> m BuilderState
    execAsmBuilderT m' = execStateT (unAsmBuilderT m') emptyBS

    concatCode :: BuilderState -> [CodeLine]
    concatCode (BS (PBS txt dat) _ _) =
      let txtCL = DirectiveCodeLine DirText : concat (reverse txt)
          datCL = DirectiveCodeLine DirData : dat
       in datCL ++ txtCL

-- | Compile the code
compile :: AsmBuilder a -> [CodeLine]
compile = runIdentity . compileT

-- | Emit main function (entry point routine)
mainFunction :: (MonadAsmBuilder m) => (() -> m ()) -> m ()
mainFunction body = do
  void $ body ()

  BS _ (FBS cls spo) _ <- getAsmBuilderState

  let globalDir = DirectiveCodeLine $ DirGlobl "_start"
  let funLabel = LabelCodeLine $ Label "_start"
  let spPush = instructionCodeLine Addi [Register Sp, Register Sp, Immediate $ -(dword * spo)]
  let loadRetVal = instructionCodeLine Li [Register A0, Immediate 0]
  let spPop = instructionCodeLine Addi [Register Sp, Register Sp, Immediate $ dword * spo]

  let ret =
        [ instructionCodeLine Li [Register A0, Immediate 0],
          instructionCodeLine Jal [Symbol "exit"]
        ]

  let funCode = [globalDir, funLabel, spPush] ++ (concat . reverse $ ([loadRetVal, spPop] ++ ret) : cls)

  pushProgramCodeLines funCode
  modifyAsmBuilderState $ \bs -> bs {functionBS = emptyFBS}

-- | Emit main function (entry point routine)
function :: (MonadAsmBuilder m) => Text -> Int64 -> ([Operand] -> m Operand) -> m Operand
function name paramCount body = do
  let args = Register <$> take (fromIntegral paramCount) [A0, A1, A2, A3, A4, A5, A6]
  memArgs <- replicateM (length args) (Memory <$> incAndGetSpo 1)
  pushFunctionCodeLines $
    (\(reg, mem) -> instructionCodeLine Sd [reg, mem]) <$> args `zip` memArgs

  retVal <- body memArgs

  BS _ (FBS cls spo) _ <- getAsmBuilderState

  let funLabel = LabelCodeLine $ Label name
  let spPush = instructionCodeLine Addi [Register Sp, Register Sp, Immediate $ -(dword * spo)]
  let loadRetVal = instructionCodeLine Ld [Register A0, retVal]
  let spPop = instructionCodeLine Addi [Register Sp, Register Sp, Immediate $ dword * spo]
  let ret = instructionCodeLine Ret []

  let funCode = [funLabel, spPush] ++ (concat . reverse $ [loadRetVal, spPop, ret] : cls)

  pushProgramCodeLines funCode
  modifyAsmBuilderState $ \bs -> bs {functionBS = emptyFBS}

  return $ Symbol name

-- | Emit uninitialized global variable
globalVar :: (MonadAsmBuilder m) => Text -> m Operand
globalVar name = do
  let gVarDir = LabeledDirectiveCodeLine (Label name) (DirDWord 0)
  modifyAsmBuilderState $
    \bs@(BS pbs _ _) ->
      bs {programBS = pbs {sectionData = gVarDir : sectionData pbs}}
  return $ Symbol name

-- | Get external function operand
externFunction :: (MonadAsmBuilder m) => Text -> m Operand
externFunction = return . Symbol

storeToLabeledAddr :: (MonadAsmBuilder m) => Operand -> Operand -> m ()
storeToLabeledAddr addr value = do
  let regA = T0
  let ra = Register regA
  let rb = Register T1

  pushFunctionCodeLines
    [ instructionCodeLine La [ra, addr],
      instructionCodeLine Ld [rb, value],
      instructionCodeLine Sd [rb, RegisterWithOffset regA 0]
    ]

  return ()

-- MONADS : START

newtype AsmBuilderT m a = AsmBuilderT {unAsmBuilderT :: StateT BuilderState m a}
  deriving (Functor, Applicative, Monad, MonadFix) via StateT BuilderState m

data BuilderState = BS
  { programBS :: ProgramBuilderState,
    functionBS :: FunctionBuilderState,
    idCnt :: Integer
  }

data ProgramBuilderState = PBS
  { sectionText :: [[CodeLine]],
    sectionData :: [CodeLine]
  }

data FunctionBuilderState = FBS
  { functionCodeLines :: [[CodeLine]],
    stackPointerOffset :: Int64 -- In double words
  }

emptyBS :: BuilderState
emptyBS = BS emptyPBS emptyFBS 0

emptyPBS :: ProgramBuilderState
emptyPBS = PBS [] []

emptyFBS :: FunctionBuilderState
emptyFBS = FBS [] 0

instance (MonadState s m) => MonadState s (AsmBuilderT m) where
  state = lift . state

instance MonadTrans AsmBuilderT where
  lift = AsmBuilderT . lift

type AsmBuilder = AsmBuilderT Identity

class (Monad m) => MonadAsmBuilder m where
  getAsmBuilderState :: m BuilderState

  modifyAsmBuilderState :: (BuilderState -> BuilderState) -> m ()

  default getAsmBuilderState ::
    (MonadTrans t, MonadAsmBuilder m1, m ~ t m1) =>
    m BuilderState
  getAsmBuilderState = lift getAsmBuilderState

  default modifyAsmBuilderState ::
    (MonadTrans t, MonadAsmBuilder m1, m ~ t m1) =>
    (BuilderState -> BuilderState) ->
    m ()
  modifyAsmBuilderState = lift . modifyAsmBuilderState

instance (Monad m) => MonadAsmBuilder (AsmBuilderT m) where
  getAsmBuilderState = AsmBuilderT get

  modifyAsmBuilderState = AsmBuilderT . modify

instance (MonadAsmBuilder m) => MonadAsmBuilder (StateT s m)

-- MONADS : END

data Operand
  = Immediate Int64
  | Register Register
  | Memory Offset
  | RegisterWithOffset Register Offset
  | Symbol Text
  deriving (Eq, Ord)

instance Show Operand where
  show :: Operand -> String
  show (Immediate i) = show i
  show (Register r) = show r
  show (Memory o) = show (dword * o) ++ "(sp)"
  show (RegisterWithOffset r o) = show (dword * o) ++ "(" ++ show r ++ ")"
  show (Symbol t) = Txt.unpack t

type Offset = Int64

immediate :: (MonadAsmBuilder m) => Int64 -> m Operand
immediate value = do
  let rd = Register T0

  rdMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines
    [ instructionCodeLine Li [rd, Immediate value],
      instructionCodeLine Sd [rd, rdMem]
    ]

  return rdMem

data CodeLine
  = LabeledDirectiveCodeLine Label Directive
  | LabelCodeLine Label
  | InstructionCodeLine Instruction
  | DirectiveCodeLine Directive

instance Show CodeLine where
  show :: CodeLine -> String
  show (LabeledDirectiveCodeLine l d) = spaceSep [show l, show d]
  show (LabelCodeLine l) = show l
  show (InstructionCodeLine i) = "    " ++ show i
  show (DirectiveCodeLine d) = show d

newtype Label = Label Text

instance Show Label where
  show :: Label -> String
  show (Label txt) = Txt.unpack txt ++ ":"

data Instruction = Instruction OpCode [Operand]

instance Show Instruction where
  show :: Instruction -> String
  show (Instruction opCode args) = spaceSep [show opCode, commaSep $ show <$> args]

data Directive
  = DirText
  | DirData
  | DirDWord Int64
  | DirGlobl Text

instance Show Directive where
  show :: Directive -> String
  show DirText = ".section .text"
  show DirData = ".section .data"
  show (DirDWord initVal) = spaceSep [".dword", show initVal]
  show (DirGlobl name) = spaceSep [".globl", Txt.unpack name]

data OpCode
  = Add
  | Sub
  | Mul
  | And
  | Or
  | Not
  | Seqz
  | Snez
  | Slt
  | Ld
  | Sd
  | Addi
  | Li
  | La
  | Neg
  | Ret
  | Jal
  | Beqz
  | J

instance Show OpCode where
  show :: OpCode -> String
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show And = "and"
  show Or = "or"
  show Not = "not"
  show Seqz = "seqz"
  show Snez = "snez"
  show Slt = "slt"
  show Ld = "ld"
  show Sd = "sd"
  show Addi = "addi"
  show Li = "li"
  show La = "la"
  show Neg = "neg"
  show Ret = "ret"
  show Jal = "jal"
  show Beqz = "beqz"
  show J = "j"

data Register
  = Zero -- zero == x0
  | Ra -- ra == x1
  | Sp -- sp == x2
  | Gp -- gp == x3
  | Tp -- tp == x4
  | T0 -- t0 == x5
  | T1 -- t1 == x6
  | T2 -- t2 == x7
  | S0 -- s0 == x8
  | S1 -- s1 == x9
  | A0 -- a0 == x10
  | A1 -- a1 == x11
  | A2 -- a2 == x12
  | A3 -- a3 == x13
  | A4 -- a4 == x14
  | A5 -- a5 == x15
  | A6 -- a6 == x16
  | A7 -- a7 == x17
  | S2 -- s2 == x18
  | S3 -- s3 == x19
  | S4 -- s4 == x20
  | S5 -- s5 == x21
  | S6 -- s6 == x22
  | S7 -- s7 == x23
  | S8 -- s8 == x24
  | S9 -- s9 == x25
  | S10 -- s10 == x26
  | S11 -- s11 == x27
  | T3 -- t3 == x28
  | T4 -- t4 == x29
  | T5 -- t5 == x30
  | T6 -- t6 == x31
  deriving (Eq, Ord)

instance Show Register where
  show :: Register -> String
  show Zero = "zero"
  show Ra = "ra"
  show Sp = "sp"
  show Gp = "gp"
  show Tp = "tp"
  show T0 = "t0"
  show T1 = "t1"
  show T2 = "t2"
  show S0 = "s0"
  show S1 = "s1"
  show A0 = "a0"
  show A1 = "a1"
  show A2 = "a2"
  show A3 = "a3"
  show A4 = "a4"
  show A5 = "a5"
  show A6 = "a6"
  show A7 = "a7"
  show S2 = "s2"
  show S3 = "s3"
  show S4 = "s4"
  show S5 = "s5"
  show S6 = "s6"
  show S7 = "s7"
  show S8 = "s8"
  show S9 = "s9"
  show S10 = "s10"
  show S11 = "s11"
  show T3 = "t3"
  show T4 = "t4"
  show T5 = "t5"
  show T6 = "t6"

-- boolean
and, or :: (MonadAsmBuilder m) => Operand -> Operand -> m Operand
and = genOpCodeInstruction1 And
or = genOpCodeInstruction1 Or

-- arithmetic
add, sub, mul :: (MonadAsmBuilder m) => Operand -> Operand -> m Operand
add = genOpCodeInstruction1 Add
sub = genOpCodeInstruction1 Sub
mul = genOpCodeInstruction1 Mul

neg :: (MonadAsmBuilder m) => Operand -> m Operand
neg a = do
  let rd = Register T0
  let rs = Register T0

  rdMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines
    [ instructionCodeLine Ld [rs, a],
      instructionCodeLine Neg [rd, rs],
      instructionCodeLine Sd [rd, rdMem]
    ]

  return rdMem

-- comparison
eq, ne, lt, le, gt, ge :: (MonadAsmBuilder m) => Operand -> Operand -> m Operand
eq = genOpCodeInstruction2 Sub Seqz
ne = genOpCodeInstruction2 Sub Snez
lt = genOpCodeInstruction1 Slt
le = flip ge
gt = flip lt
ge = genOpCodeInstruction2 Slt Seqz

call :: (MonadAsmBuilder m) => Operand -> [Operand] -> m Operand
call fun args = do
  let argRegs = Register <$> [A0, A1, A2, A3, A4, A5, A6]
  let loadArguments = uncurry loadArgCL <$> argRegs `zip` args

  let retVal = Register A0
  retValMem <- Memory <$> incAndGetSpo 1
  raMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines $
    loadArguments
      ++ [ instructionCodeLine Sd [Register Ra, raMem],
           instructionCodeLine Jal [fun],
           instructionCodeLine Ld [Register Ra, raMem],
           instructionCodeLine Sd [retVal, retValMem]
         ]

  return retValMem
  where
    loadArgCL reg arg = case arg of
      Symbol _ -> instructionCodeLine La [reg, arg]
      _ -> instructionCodeLine Ld [reg, arg]

ite :: (MonadAsmBuilder m) => Operand -> (() -> m Operand) -> (() -> m Operand) -> m Operand
ite c t e = do
  let retVal = Register T0
  retValMem <- Memory <$> incAndGetSpo 1

  elseLabelName <- Txt.pack . ("else_" ++) . show <$> genId
  endLabelName <- Txt.pack . ("end_" ++) . show <$> genId

  -- condition
  let condReg = Register T1
  let loadCond = instructionCodeLine Ld [condReg, c]
  let br = instructionCodeLine Beqz [condReg, Symbol elseLabelName]

  pushFunctionCodeLines [loadCond, br]

  -- then
  storeThenRes <- (\op -> [instructionCodeLine Ld [retVal, op], instructionCodeLine Sd [retVal, retValMem]]) <$> t ()
  let jumpFromThenToEnd = instructionCodeLine J [Symbol endLabelName]

  pushFunctionCodeLines $ storeThenRes ++ [jumpFromThenToEnd]

  -- else
  let elseLabel = LabelCodeLine $ Label elseLabelName
  pushFunctionCodeLines [elseLabel]

  storeElseRes <- (\op -> [instructionCodeLine Ld [retVal, op], instructionCodeLine Sd [retVal, retValMem]]) <$> e ()

  pushFunctionCodeLines storeElseRes

  -- end
  let endLabel = LabelCodeLine $ Label endLabelName

  pushFunctionCodeLines [endLabel]

  return retValMem

-- Utils

genOpCodeInstruction1 :: (MonadAsmBuilder m) => OpCode -> Operand -> Operand -> m Operand
genOpCodeInstruction1 opCode a b = do
  let rd = Register T0
  let rs1 = Register T0
  let rs2 = Register T1

  rdMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines
    [ instructionCodeLine Ld [rs1, a],
      instructionCodeLine Ld [rs2, b],
      instructionCodeLine opCode [rd, rs1, rs2],
      instructionCodeLine Sd [rd, rdMem]
    ]

  return rdMem

genOpCodeInstruction2 :: (MonadAsmBuilder m) => OpCode -> OpCode -> Operand -> Operand -> m Operand
genOpCodeInstruction2 opCode1 opCode2 a b = do
  let rd = Register T0
  let rs1 = Register T0
  let rs2 = Register T1

  rdMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines
    [ instructionCodeLine Ld [rs1, a],
      instructionCodeLine Ld [rs2, b],
      instructionCodeLine opCode1 [rd, rs1, rs2],
      instructionCodeLine opCode2 [rd, rd],
      instructionCodeLine Sd [rd, rdMem]
    ]

  return rdMem

pushProgramCodeLines :: (MonadAsmBuilder m) => [CodeLine] -> m ()
pushProgramCodeLines newCodeLines = modifyAsmBuilderState $
  \bs@(BS pbs@(PBS currCodeLines _) _ _) ->
    bs {programBS = pbs {sectionText = newCodeLines : currCodeLines}}

pushFunctionCodeLines :: (MonadAsmBuilder m) => [CodeLine] -> m ()
pushFunctionCodeLines newCodeLines = modifyAsmBuilderState $
  \bs@(BS _ fbs@(FBS currCodeLines _) _) ->
    bs {functionBS = fbs {functionCodeLines = newCodeLines : currCodeLines}}

incAndGetSpo :: (MonadAsmBuilder m) => Int64 -> m Int64
incAndGetSpo amount = do
  spo <- stackPointerOffset . functionBS <$> getAsmBuilderState
  let newSpo = spo + amount

  modifyAsmBuilderState $
    \bs@(BS _ fbs _) ->
      bs {functionBS = fbs {stackPointerOffset = newSpo}}

  return newSpo

instructionCodeLine :: OpCode -> [Operand] -> CodeLine
instructionCodeLine opCode ops = InstructionCodeLine $ Instruction opCode ops

genId :: (MonadAsmBuilder m) => m Integer
genId = do
  cnt <- idCnt <$> getAsmBuilderState
  modifyAsmBuilderState $ \bs -> bs {idCnt = cnt + 1}
  return cnt

dword :: Int64
dword = 8

commaSep :: [String] -> String
commaSep = intercalate ", " . filterOutBlankStrings

spaceSep :: [String] -> String
spaceSep = unwords . filterOutBlankStrings

filterOutBlankStrings :: [String] -> [String]
filterOutBlankStrings = filter (not . all isSpace)
