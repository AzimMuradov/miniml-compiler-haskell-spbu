{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CodeGen.Llvm.Ir2LlvmIr (ppLlvm, ir2LlvmIr) where

import CodeGen.Module (Module (..))
import Control.Monad.State (MonadFix)
import Data.String.Transform (toShortByteString)
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
import Trees.Common

ppLlvm :: LLVM.Module -> Text
ppLlvm = ppllvm

ir2LlvmIr :: Module -> LLVM.Module
ir2LlvmIr = genModule

-- Implementation

genModule :: Module -> LLVM.Module
genModule (Module name code) = LLVM.buildModule (toShortByteString name) undefined

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
  AtomUnit -> return $ LLVM.int64 0
  AtomBool bool -> return $ LLVM.int64 $ fromBool bool
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
          CompOp cOp ->
            let cOpF = case cOp of
                  EqOp -> LLVM.icmp LLVM.EQ
                  NeOp -> LLVM.icmp LLVM.NE
                  LtOp -> LLVM.icmp LLVM.SLT
                  LeOp -> LLVM.icmp LLVM.SLE
                  GtOp -> LLVM.icmp LLVM.SGT
                  GeOp -> LLVM.icmp LLVM.SGE
             in (\a b -> cOpF a b >>= boolToInt)
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
  Expression ->
  Expression ->
  m LLVM.Operand
genIte c t e = mdo
  rv <- LLVM.alloca LLVM.i64 Nothing 0

  c' <- genAtom c >>= intToBool
  LLVM.condBr c' tBlock eBlock

  tBlock <- LLVM.block `LLVM.named` "if.then"
  store' rv =<< genExpr t
  LLVM.br end

  eBlock <- LLVM.block `LLVM.named` "if.else"
  store' rv =<< genExpr e
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

-- Utils

boolToInt ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  (LLVM.Operand -> m LLVM.Operand)
boolToInt = flip LLVM.zext LLVM.i64

intToBool ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  (LLVM.Operand -> m LLVM.Operand)
intToBool = flip LLVM.trunc LLVM.i64
