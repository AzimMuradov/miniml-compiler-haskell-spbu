{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Llvm.Ir2LlvmIr (ppLlvmModule, genLlvmIrModule) where

import CodeGen.Module (Module (Module))
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Transform (toShortByteString)
import qualified Data.Text as Text
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
import Transformations.Anf.Anf
import Trees.Common
import Utils (locally)

ppLlvmModule :: LLVM.Module -> Text
ppLlvmModule = ppllvm

genLlvmIrModule :: Module -> LLVM.Module
genLlvmIrModule = genModule

-- Implementation

type CodeGenM = LLVM.IRBuilderT Llvm

type Llvm = LLVM.ModuleBuilderT (State Env)

data Env = Env
  { locVars :: Map Identifier' LLVM.Operand,
    globVars :: Map Identifier' LLVM.Operand,
    funs :: Map Identifier' LLVM.Operand
  }

genModule :: Module -> LLVM.Module
genModule (Module name (Program decls)) = flip evalState (Env Map.empty Map.empty Map.empty) $
  LLVM.buildModuleT (toShortByteString name) $ do
    notF <- LLVM.extern "not" [LLVM.i64] LLVM.i64
    printBoolF <- LLVM.extern "print_bool" [LLVM.i64] LLVM.i64
    printIntF <- LLVM.extern "print_int" [LLVM.i64] LLVM.i64

    let stdFuns =
          [ (Txt "not", notF),
            (Txt "print_bool", printBoolF),
            (Txt "print_int", printIntF)
          ]

    mapM_ (uncurry regFun) stdFuns

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
    regFun ident fun
    fun <- locally $ do
      LLVM.function
        (LLVM.mkName $ genId ident)
        ((LLVM.i64,) . LLVM.ParameterName . toShortByteString . genId <$> params)
        LLVM.i64
        $ \args -> do
          mapM_ (uncurry regVar) (params `zip` args)
          body' <- genExpr body
          LLVM.ret body'
    return ()

genId :: Identifier' -> String
genId = \case
  Txt txt -> Text.unpack txt
  Gen n txt -> Text.unpack txt <> "'" <> show n

genExpr :: Expression -> CodeGenM LLVM.Operand
genExpr = \case
  ExprAtom atom -> genAtom atom
  ExprComp ce -> genComp ce
  ExprLetIn (ident, val) expr -> mdo
    val' <- genExpr val `LLVM.named` toShortByteString (genId ident)
    regVar ident val'
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

genComp :: ComplexExpression -> CodeGenM LLVM.Operand
genComp = \case
  CompApp f arg -> mdo
    f' <- findFun f
    arg' <- genAtom arg
    LLVM.call f' [(arg', [])]
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

-- Stack

findVar :: Identifier' -> CodeGenM LLVM.Operand
findVar k = do
  locVar <- gets ((Map.!? k) . locVars)
  maybe (load' =<< findGlobVar k) return locVar

findGlobVar :: MonadState Env m => Identifier' -> m LLVM.Operand
findGlobVar k = gets ((Map.! k) . globVars)

regVar :: MonadState Env m => Identifier' -> LLVM.Operand -> m ()
regVar k v = modify $ \env -> env {locVars = Map.insert k v (locVars env)}

regGlobVar :: MonadState Env m => Identifier' -> LLVM.Operand -> m ()
regGlobVar k v = modify $ \env -> env {globVars = Map.insert k v (globVars env)}

findFun :: Identifier' -> CodeGenM LLVM.Operand
findFun k = gets ((Map.! k) . funs)

regFun :: MonadState Env m => Identifier' -> LLVM.Operand -> m ()
regFun k v = modify $ \env -> env {funs = Map.insert k v (funs env)}

-- Allocation utils

allocate :: LLVM.Operand -> CodeGenM LLVM.Operand
allocate value = do
  addr <- LLVM.alloca LLVM.i64 Nothing 0
  store' addr value
  return addr

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
