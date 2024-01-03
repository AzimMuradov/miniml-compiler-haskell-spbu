{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Llvm.Ir2LlvmIr (ppLlvmModule, genLlvmIrModule) where

import CodeGen.Module (Module (Module))
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.Functor.Foldable (ListF (Cons, Nil), hylo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Transform (toShortByteString)
import qualified Data.Text as Txt
import Data.Text.Lazy (Text)
import Foreign (fromBool)
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import LLVM.Pretty (ppllvm)
import MonadUtils (locally)
import qualified StdLib
import Transformations.Anf.Anf
import Trees.Common

-- * LLVM Code Generation

genLlvmIrModule :: Module -> LLVM.Module
genLlvmIrModule = genModule

ppLlvmModule :: LLVM.Module -> Text
ppLlvmModule = ppllvm

-- * Implementation

type CodeGenM = LLVM.IRBuilderT Llvm

type Llvm = LLVM.ModuleBuilderT (State Env)

data Env = Env
  { locVars :: Map Identifier' LLVM.Operand,
    globVars :: Map Identifier' LLVM.Operand,
    funs :: Map Identifier' (LLVM.Operand, ParamsCnt)
  }

type ParamsCnt = Int

genModule :: Module -> LLVM.Module
genModule (Module name (Program decls)) = flip evalState (Env Map.empty Map.empty Map.empty) $
  LLVM.buildModuleT (toShortByteString name) $ do
    mapM_ regStdLibDecl StdLib.allTypedDecls
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

genGlobDecl :: GlobalDeclaration -> Llvm ()
genGlobDecl = \case
  GlobVarDecl ident _ -> do
    var <- LLVM.global (LLVM.mkName $ genId ident) LLVM.i64 (C.Int 64 0)
    regGlobVar ident var
  GlobFunDecl ident params body -> mdo
    regFun ident fun (length params)
    fun <- locally $ mdo
      LLVM.function
        (LLVM.mkName $ genId ident)
        ((LLVM.i64,) . LLVM.ParameterName . toShortByteString . genId <$> params)
        LLVM.i64
        $ \args -> mdo
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
    val' <- genExpr val `LLVM.named` toShortByteString (genId ident)
    regLocVar ident val'
    genExpr expr

genAtom :: AtomicExpression -> CodeGenM LLVM.Operand
genAtom = \case
  AtomId ident -> findVar ident
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
          ArithOp DivOp ->
            ( \lhs'' rhs'' -> do
                divF <- findFun (Txt "miniml_div")
                LLVM.call divF [(lhs'', []), (rhs'', [])]
            )
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

genComp :: ComplexExpression -> CodeGenM LLVM.Operand
genComp = \case
  CompApp f arg -> do
    f' <- findPaf f
    arg' <- genAtom arg
    applyF <- findFun (Txt "miniml_apply")
    LLVM.call applyF [(f', []), (arg', [])]
  CompIte c t e -> mdo
    rv <- allocate'

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

-- Vars

findVar :: Identifier' -> CodeGenM LLVM.Operand
findVar ident = do
  locVar <- findLocVar ident
  case locVar of
    Just locVar' -> return locVar'
    Nothing -> do
      gFun <- gets ((Map.!? ident) . funs)
      case gFun of
        Just (fun, pCnt) -> do
          funToPafF <- findFun (Txt "miniml_fun_to_paf")
          LLVM.call funToPafF [(fun, []), (LLVM.int64 (toInteger pCnt), [])]
        Nothing -> load' =<< findGlobVar ident

findLocVar :: Identifier' -> CodeGenM (Maybe LLVM.Operand)
findLocVar ident = gets ((Map.!? ident) . locVars)

findGlobVar :: MonadState Env m => Identifier' -> m LLVM.Operand
findGlobVar ident = do
  a <- gets ((Map.!? ident) . globVars)
  maybe (error $ show ident) return a

regLocVar :: MonadState Env m => Identifier' -> LLVM.Operand -> m ()
regLocVar ident var = modify $
  \env -> env {locVars = Map.insert ident var (locVars env)}

regGlobVar :: MonadState Env m => Identifier' -> LLVM.Operand -> m ()
regGlobVar ident gVar = modify $
  \env -> env {globVars = Map.insert ident gVar (globVars env)}

-- Funs

findPaf :: Identifier' -> CodeGenM LLVM.Operand
findPaf ident = do
  locVar <- findLocVar ident
  case locVar of
    Just locVar' -> return locVar'
    Nothing -> do
      a <- gets ((Map.!? ident) . funs)
      (fun, pCnt) <- maybe (error $ show ident) return a
      funToPafF <- findFun (Txt "miniml_fun_to_paf")
      LLVM.call funToPafF [(fun, []), (LLVM.int64 (toInteger pCnt), [])]

findFun :: Identifier' -> CodeGenM LLVM.Operand
findFun ident = do
  a <- gets ((Map.!? ident) . funs)
  b <- maybe (error $ show ident) return a
  return $ fst b

regFun :: MonadState Env m => Identifier' -> LLVM.Operand -> ParamsCnt -> m ()
regFun ident fun paramsCnt = modify $
  \env -> env {funs = Map.insert ident (fun, paramsCnt) (funs env)}

-- StdLib utils

regStdLibDecl :: StdLib.TypedDeclaration -> Llvm ()
regStdLibDecl decl = register decl =<< declareAsExtern decl
  where
    declareAsExtern :: StdLib.TypedDeclaration -> Llvm LLVM.Operand
    declareAsExtern (ident, t) =
      LLVM.extern
        (LLVM.mkName $ Txt.unpack ident)
        (replicate (paramsTypesCount t) LLVM.i64)
        LLVM.i64

    register :: StdLib.TypedDeclaration -> LLVM.Operand -> Llvm ()
    register (ident, t) fun = regFun (Txt ident) fun (paramsTypesCount t)

    paramsTypesCount :: Type -> Int
    paramsTypesCount = hylo paramsTypesCount'' paramsTypesCount'

    paramsTypesCount' :: Type -> ListF Type Type
    paramsTypesCount' (TFun pT retT) = Cons pT retT
    paramsTypesCount' _ = Nil

    paramsTypesCount'' :: ListF Type Int -> Int
    paramsTypesCount'' (Cons _ n) = n + 1
    paramsTypesCount'' Nil = 0

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
