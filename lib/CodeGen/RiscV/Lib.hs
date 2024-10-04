{-# LANGUAGE OverloadedStrings #-}

module CodeGen.RiscV.Lib
  ( compileT,
    compile,
    ppCodeLines,
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

import CodeGen.RiscV.Lib.Monad
import CodeGen.RiscV.Lib.Types
import Control.Monad (replicateM, void)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (execStateT)
import Data.Int (Int64)
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

immediate :: (MonadAsmBuilder m) => Int64 -> m Operand
immediate value = do
  let rd = Register T0

  rdMem <- Memory <$> incAndGetSpo 1

  pushFunctionCodeLines
    [ instructionCodeLine Li [rd, Immediate value],
      instructionCodeLine Sd [rd, rdMem]
    ]

  return rdMem

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
