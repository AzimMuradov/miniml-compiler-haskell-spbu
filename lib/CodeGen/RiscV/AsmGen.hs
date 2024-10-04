{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.RiscV.AsmGen (ppRiscVAsm) where

import CodeGen.Module (Module (..))
import qualified CodeGen.RiscV.Lib as Asm
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Txt
import Foreign (fromBool)
import MonadUtils (locally)
import qualified StdLib
import Transformations.Anf.Anf
import Trees.Common

ppRiscVAsm :: Module -> String
ppRiscVAsm m = unlines $ show <$> genModule m

-- The Code

type CodeGenM = Asm.AsmBuilderT (State Env)

data Env = Env
  { locVars :: Map Identifier' Asm.Operand,
    globVars :: Map Identifier' Asm.Operand,
    funs :: Map Identifier' (Asm.Operand, Arity)
  }

genModule :: Module -> [Asm.CodeLine]
genModule (Module (Program decls)) = flip evalState (Env Map.empty Map.empty Map.empty) $
  Asm.compileT $ do
    mapM_ genStdLibDecl StdLib.allDeclsWithArity
    mapM_ genGlobDecl decls

    -- In the `main` we define our global variables.
    Asm.mainFunction $ \_ -> mapM_ gVarDef decls
  where
    gVarDef :: GlobalDeclaration -> CodeGenM ()
    gVarDef = \case
      GlobVarDecl ident value -> do
        addr <- findGlobVar ident
        value' <- genExpr value
        Asm.storeToLabeledAddr addr value'
      _ -> return ()

genStdLibDecl :: StdLib.DeclarationWithArity -> CodeGenM ()
genStdLibDecl decl = declareAsExtern decl >>= register decl
  where
    declareAsExtern :: StdLib.DeclarationWithArity -> CodeGenM Asm.Operand
    declareAsExtern (ident, _) = Asm.externFunction ident

    register :: StdLib.DeclarationWithArity -> Asm.Operand -> CodeGenM ()
    register (ident, arity) fun = regFun (Txt ident) fun arity

genGlobDecl :: GlobalDeclaration -> CodeGenM ()
genGlobDecl = \case
  GlobVarDecl ident _ -> do
    var <- Asm.globalVar (Txt.pack $ genId ident)
    regGlobVar ident var
  GlobFunDecl ident params body -> mdo
    regFun ident fun (length params)
    fun <- locally $ do
      Asm.function
        (Txt.pack $ genId ident)
        (fromIntegral $ length params)
        $ \args -> do
          mapM_ (uncurry regLocVar) (params `zip` args)
          genExpr body
    return ()

genId :: Identifier' -> String
genId = \case
  Txt txt -> Txt.unpack txt
  Gen n txt -> Txt.unpack txt <> "_" <> show n

genExpr :: Expression -> CodeGenM Asm.Operand
genExpr = \case
  ExprAtom atom -> genAtom atom
  ExprComp ce -> genComp ce
  ExprLetIn (ident, val) expr -> do
    val' <- genExpr val
    regLocVar ident val'
    genExpr expr

genAtom :: AtomicExpression -> CodeGenM Asm.Operand
genAtom = \case
  AtomId ident -> findAny ident
  AtomUnit -> Asm.immediate 0
  AtomBool bool -> Asm.immediate $ fromBool bool
  AtomInt int -> Asm.immediate int

genComp :: ComplexExpression -> CodeGenM Asm.Operand
genComp = \case
  CompApp f arg -> do
    f' <- findAny f
    arg' <- genAtom arg
    applyF <- findFun (Txt "miniml_apply")
    Asm.call applyF [f', arg']
  CompIte c t e -> do
    c' <- genAtom c
    Asm.ite c' (\_ -> genExpr t) (\_ -> genExpr e)
  CompBinOp op lhs rhs -> do
    lhs' <- genAtom lhs
    rhs' <- genAtom rhs
    let opF = case op of
          BoolOp AndOp -> Asm.and
          BoolOp OrOp -> Asm.or
          ArithOp PlusOp -> Asm.add
          ArithOp MinusOp -> Asm.sub
          ArithOp MulOp -> Asm.mul
          ArithOp DivOp ->
            ( \lhs'' rhs'' -> do
                divF <- findFun (Txt "miniml_div")
                Asm.call divF [lhs'', rhs'']
            )
          CompOp EqOp -> Asm.eq
          CompOp NeOp -> Asm.ne
          CompOp LtOp -> Asm.lt
          CompOp LeOp -> Asm.le
          CompOp GtOp -> Asm.gt
          CompOp GeOp -> Asm.ge
    opF lhs' rhs'
  CompUnOp op x -> do
    x' <- genAtom x
    let opF = case op of
          UnMinusOp -> Asm.neg
    opF x'

-- Vars & Funs

findAny :: Identifier' -> CodeGenM Asm.Operand
findAny ident = do
  maybeLocVar <- gets ((Map.!? ident) . locVars)
  case maybeLocVar of
    Just locVar -> return locVar
    Nothing -> do
      maybeFun <- gets ((Map.!? ident) . funs)
      case maybeFun of
        Just (fun, arity) -> do
          funToPafF <- findFun (Txt "miniml_fun_to_paf")
          arity' <- Asm.immediate $ fromIntegral arity
          Asm.call funToPafF [fun, arity']
        Nothing -> findGlobVar ident

findGlobVar :: (MonadState Env m) => Identifier' -> m Asm.Operand
findGlobVar ident = gets ((Map.! ident) . globVars)

findFun :: Identifier' -> CodeGenM Asm.Operand
findFun ident = gets (fst . (Map.! ident) . funs)

regLocVar :: (MonadState Env m) => Identifier' -> Asm.Operand -> m ()
regLocVar ident var = modify $
  \env -> env {locVars = Map.insert ident var (locVars env)}

regGlobVar :: (MonadState Env m) => Identifier' -> Asm.Operand -> m ()
regGlobVar ident gVar = modify $
  \env -> env {globVars = Map.insert ident gVar (globVars env)}

regFun :: (MonadState Env m) => Identifier' -> Asm.Operand -> Arity -> m ()
regFun ident fun paramsCnt = modify $
  \env -> env {funs = Map.insert ident (fun, paramsCnt) (funs env)}
