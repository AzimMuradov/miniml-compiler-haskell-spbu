{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CodeGen.Llvm.Ir2LlvmIr (ppLlvm, ir2LlvmIr) where

import Control.Monad (void)
import Control.Monad.State (MonadFix, MonadState (get), StateT, evalStateT)
import Data.List.NonEmpty (toList)
import Data.Map (Map, (!))
import Data.Text.Lazy (Text)
import Foreign (fromBool)
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import LLVM.Pretty (ppllvm)
import Transformations.Anf.Anf
import Trees.Common (ArithmeticOperator (..), BinaryOperator (..), BooleanOperator (..), ComparisonOperator (..), Identifier', UnaryOperator (..))

ppLlvm :: Text
ppLlvm = ppllvm ir2LlvmIr

-- ir2LlvmIr :: Program -> a
ir2LlvmIr :: LLVM.Module
ir2LlvmIr = LLVM.buildModule "example" $ do
  printInt <- LLVM.extern "print_int" [LLVM.i64] LLVM.i64

  -- create a function called `main` that will be the entry point to our program
  LLVM.function "main" [] LLVM.i64 $ \_ -> do
    -- build the LLVM AST for our expression
    ourExpression <-
      genIte
        (AtomBool True)
        (AtomBinOp (ArithOp PlusOp) (AtomInt 4) (AtomInt 8))
        (AtomInt 15)

    -- print our result to stdout
    _ <- LLVM.call printInt [(ourExpression, [])]

    -- return success exit code of `0`
    LLVM.ret (LLVM.int64 0)

-- Implementation

type CodeGenM a = LLVM.IRBuilder a

data CompilerState = CompilerState
  { stack :: Map Identifier' LLVM.Operand,
    runtime :: ()
  }

genExpr ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  Expression ->
  m LLVM.Operand
genExpr expr = case expr of
  ExprAtom atom -> genAtom atom
  ExprComp ce -> genComp ce
  ExprLetIn (ident, value) expr -> undefined

genAtom ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  AtomicExpression ->
  m LLVM.Operand
genAtom atom = case atom of
  AtomId ident -> undefined {- mdo
                            CompilerState map _ <- get
                            load' $ map ! name -}
  AtomBool bool -> return $ LLVM.bit $ fromBool bool
  AtomInt int -> return $ LLVM.int64 $ toInteger int
  AtomBinOp op lhs rhs -> do
    lhs' <- genAtom lhs
    rhs' <- genAtom rhs
    let opF = case op of
          BoolOp AndOp -> LLVM.and
          BoolOp OrOp -> LLVM.or
          ArithOp PlusOp -> LLVM.add
          ArithOp MinusOp -> LLVM.sub
          ArithOp MulOp -> LLVM.mul
          ArithOp DivOp -> LLVM.sdiv
          CompOp EqOp -> LLVM.icmp LLVM.EQ
          CompOp NeOp -> LLVM.icmp LLVM.NE
          CompOp LtOp -> LLVM.icmp LLVM.SLT
          CompOp LeOp -> LLVM.icmp LLVM.SLE
          CompOp GtOp -> LLVM.icmp LLVM.SGT
          CompOp GeOp -> LLVM.icmp LLVM.SGE
    opF lhs' rhs'
  AtomUnOp op x -> do
    x' <- genAtom x
    let opF = case op of
          UnMinusOp -> LLVM.mul (LLVM.int64 (-1))
    opF x'

genComp ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  ComplexExpression ->
  m LLVM.Operand
genComp comp = case comp of
  CompApp f arg -> undefined
  CompIte c t e -> genIte c t e

genIte ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  AtomicExpression ->
  AtomicExpression ->
  AtomicExpression ->
  m LLVM.Operand
genIte c t e = mdo
  LLVM.br begin
  begin <- LLVM.block `LLVM.named` "if.begin"

  rv <- LLVM.alloca LLVM.i64 Nothing 0

  c' <- genAtom c
  LLVM.condBr c' tBlock eBlock

  tBlock <- LLVM.block `LLVM.named` "if.then"
  store' rv =<< genAtom t
  LLVM.br end

  eBlock <- LLVM.block `LLVM.named` "if.else"
  store' rv =<< genAtom e
  LLVM.br end

  end <- LLVM.block `LLVM.named` "if.end"

  load' rv

allocate :: (LLVM.MonadIRBuilder m) => LLVM.Operand -> m LLVM.Operand
allocate value = do
  addr <- LLVM.alloca LLVM.i64 (Just (LLVM.int64 0)) 0
  store' addr value
  pure addr

load' :: (LLVM.MonadIRBuilder m) => LLVM.Operand -> m LLVM.Operand
load' addr = LLVM.load addr 0

store' :: (LLVM.MonadIRBuilder m) => LLVM.Operand -> LLVM.Operand -> m ()
store' addr = LLVM.store addr 0
