{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Llvm.LlvmIrGen (ppLlvmModule, genLlvmIrModule) where

import CodeGen.Module (Module (Module))
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Txt
import Foreign (fromBool)
import qualified LLVM.Codegen as LLVM
import MonadUtils (locally)
import qualified StdLib
import Transformations.Anf.Anf
import Trees.Common

-- * LLVM Code Generation

genLlvmIrModule :: Module -> LLVM.Module
genLlvmIrModule = genModule

ppLlvmModule :: LLVM.Module -> Text
ppLlvmModule = cs . LLVM.ppllvm

-- * Implementation

type CodeGenM = LLVM.IRBuilderT Llvm

type Llvm = LLVM.ModuleBuilderT (State Env)

data Env = Env
  { locVars :: Map Identifier' LLVM.Operand,
    globVars :: Map Identifier' LLVM.Operand,
    funs :: Map Identifier' (LLVM.Operand, Arity)
  }

genModule :: Module -> LLVM.Module
genModule (Module (Program decls)) = flip evalState (Env Map.empty Map.empty Map.empty) $
  LLVM.runModuleBuilderT $ do
    mapM_ genStdLibDecl StdLib.allDeclsWithArity
    mapM_ genGlobDecl decls

    -- In the `main` we define our global variables.
    LLVM.function "main" [] LLVM.i64 $ \_ -> do
      mapM_ gVarDef decls
      LLVM.ret (LLVM.int64 0)
  where
    gVarDef :: GlobalDeclaration -> CodeGenM ()
    gVarDef = \case
      GlobVarDecl ident value -> do
        operand <- findGlobVar ident
        value' <- genExpr value
        store' operand value'
      _ -> return ()

genStdLibDecl :: StdLib.DeclarationWithArity -> Llvm ()
genStdLibDecl decl = declareAsExtern decl >>= register decl
  where
    declareAsExtern :: StdLib.DeclarationWithArity -> Llvm LLVM.Operand
    declareAsExtern (ident, arity) =
      LLVM.extern
        (LLVM.Name ident)
        (replicate arity LLVM.i64)
        LLVM.i64

    register :: StdLib.DeclarationWithArity -> LLVM.Operand -> Llvm ()
    register (ident, arity) fun = regFun (Txt ident) fun arity

genGlobDecl :: GlobalDeclaration -> Llvm ()
genGlobDecl = \case
  GlobVarDecl ident _ -> do
    var <- LLVM.global (LLVM.Name $ Txt.pack $ genId ident) LLVM.i64 (LLVM.Int 64 0)
    regGlobVar ident var
  GlobFunDecl ident params body -> mdo
    regFun ident fun (length params)
    fun <- locally $ do
      LLVM.function
        (LLVM.Name $ Txt.pack $ genId ident)
        ((LLVM.i64,) . LLVM.ParameterName . Txt.pack . genId <$> params)
        LLVM.i64
        $ \args -> do
          mapM_ (uncurry regLocVar) (params `zip` args)
          body' <- genExpr body
          LLVM.ret body'
    return ()

genId :: Identifier' -> String
genId = \case
  Txt txt -> Txt.unpack txt
  Gen n txt -> Txt.unpack txt <> "." <> show n

genExpr :: Expression -> CodeGenM LLVM.Operand
genExpr = \case
  ExprAtom atom -> genAtom atom
  ExprComp ce -> genComp ce
  ExprLetIn (ident, val) expr -> do
    val' <- genExpr val
    regLocVar ident val'
    genExpr expr

genAtom :: AtomicExpression -> CodeGenM LLVM.Operand
genAtom = \case
  AtomId ident -> findAny ident
  AtomUnit -> return $ LLVM.int64 0
  AtomBool bool -> return $ LLVM.int64 $ fromBool bool
  AtomInt int -> return $ LLVM.int64 $ toInteger int

genComp :: ComplexExpression -> CodeGenM LLVM.Operand
genComp = \case
  CompApp f arg -> do
    f' <- findAny f
    arg' <- genAtom arg
    applyF <- findFun (Txt "miniml_apply")
    LLVM.call applyF [f', arg']
  CompIte c t e -> mdo
    rv <- allocate'

    c' <- genAtom c >>= intToBool
    LLVM.condBr c' tBlock eBlock

    tBlock <- LLVM.blockNamed "if.then"
    store' rv =<< genExpr t
    LLVM.br end

    eBlock <- LLVM.blockNamed "if.else"
    store' rv =<< genExpr e
    LLVM.br end

    end <- LLVM.blockNamed "if.end"

    load' rv
  CompBinOp op lhs rhs -> do
    lhs' <- genAtom lhs
    rhs' <- genAtom rhs
    let opF = case op of
          BoolOp AndOp -> LLVM.and
          BoolOp OrOp -> LLVM.or
          ArithOp PlusOp -> LLVM.add
          ArithOp MinusOp -> LLVM.sub
          ArithOp MulOp -> LLVM.mul
          ArithOp DivOp -> \lhs'' rhs'' -> do
            divF <- findFun (Txt "miniml_div")
            LLVM.call divF [lhs'', rhs'']
          CompOp cOp ->
            let cOpF = case cOp of
                  EqOp -> LLVM.eq
                  NeOp -> LLVM.ne
                  LtOp -> LLVM.slt
                  LeOp -> LLVM.sle
                  GtOp -> LLVM.sgt
                  GeOp -> LLVM.sge
             in (\a b -> cOpF a b >>= boolToInt)
    opF lhs' rhs'
  CompUnOp op x -> do
    x' <- genAtom x
    let opF = case op of
          UnMinusOp -> LLVM.mul (LLVM.int64 (-1))
    opF x'

-- Vars & Funs

findAny :: Identifier' -> CodeGenM LLVM.Operand
findAny ident = do
  maybeLocVar <- gets ((Map.!? ident) . locVars)
  case maybeLocVar of
    Just locVar -> return locVar
    Nothing -> do
      maybeFun <- gets ((Map.!? ident) . funs)
      case maybeFun of
        Just (fun, arity) -> do
          funToPafF <- findFun (Txt "miniml_fun_to_paf")
          fun' <- LLVM.ptrtoint fun LLVM.i64
          LLVM.call funToPafF [fun', LLVM.int64 (toInteger arity)]
        Nothing -> load' =<< findGlobVar ident

findGlobVar :: (MonadState Env m) => Identifier' -> m LLVM.Operand
findGlobVar ident = gets ((Map.! ident) . globVars)

findFun :: Identifier' -> CodeGenM LLVM.Operand
findFun ident = gets (fst . (Map.! ident) . funs)

regLocVar :: (MonadState Env m) => Identifier' -> LLVM.Operand -> m ()
regLocVar ident var = modify $
  \env -> env {locVars = Map.insert ident var (locVars env)}

regGlobVar :: (MonadState Env m) => Identifier' -> LLVM.Operand -> m ()
regGlobVar ident gVar = modify $
  \env -> env {globVars = Map.insert ident gVar (globVars env)}

regFun :: (MonadState Env m) => Identifier' -> LLVM.Operand -> Arity -> m ()
regFun ident fun paramsCnt = modify $
  \env -> env {funs = Map.insert ident (fun, paramsCnt) (funs env)}

-- Allocation utils

allocate' :: CodeGenM LLVM.Operand
allocate' = LLVM.alloca LLVM.i64 Nothing 0

load' :: LLVM.Operand -> CodeGenM LLVM.Operand
load' addr = LLVM.load addr 0

store' :: LLVM.Operand -> LLVM.Operand -> CodeGenM ()
store' addr = LLVM.store addr 0

-- Conversion utils

boolToInt :: LLVM.Operand -> CodeGenM LLVM.Operand
boolToInt = flip LLVM.zext LLVM.i64

intToBool :: LLVM.Operand -> CodeGenM LLVM.Operand
intToBool = flip LLVM.trunc LLVM.i1
