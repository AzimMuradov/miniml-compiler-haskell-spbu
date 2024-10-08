{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.RiscV.AsmGen (ppRiscVAsm) where

import CodeGen.Module (Module (..))
import qualified CodeGen.RiscV.Lib as Rv
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Txt
import Foreign (fromBool)
import MonadUtils (locally)
import qualified StdLib
import Transformations.Anf.Anf
import Trees.Common

ppRiscVAsm :: Module -> Text
ppRiscVAsm m = Rv.ppCodeLines $ genModule m

-- The Code

type CodeGenM = Rv.AsmBuilderT (State Env)

data Env = Env
  { locVars :: Map Identifier' Rv.Operand,
    globVars :: Map Identifier' Rv.Operand,
    funs :: Map Identifier' (Rv.Operand, Arity)
  }

genModule :: Module -> [Rv.CodeLine]
genModule (Module (Program decls)) = flip evalState (Env Map.empty Map.empty Map.empty) $
  Rv.compileT $ do
    mapM_ genStdLibDecl StdLib.allDeclsWithArity
    mapM_ genGlobDecl decls

    -- In the `main` we define our global variables.
    Rv.mainFunction $ \_ -> mapM_ gVarDef decls
  where
    gVarDef :: GlobalDeclaration -> CodeGenM ()
    gVarDef = \case
      GlobVarDecl ident value -> do
        addr <- findGlobVar ident
        value' <- genExpr value
        Rv.storeToLabeledAddr addr value'
      _ -> return ()

genStdLibDecl :: StdLib.DeclarationWithArity -> CodeGenM ()
genStdLibDecl decl = declareAsExtern decl >>= register decl
  where
    declareAsExtern :: StdLib.DeclarationWithArity -> CodeGenM Rv.Operand
    declareAsExtern (ident, _) = Rv.externFunction ident

    register :: StdLib.DeclarationWithArity -> Rv.Operand -> CodeGenM ()
    register (ident, arity) fun = regFun (Txt ident) fun arity

genGlobDecl :: GlobalDeclaration -> CodeGenM ()
genGlobDecl = \case
  GlobVarDecl ident _ -> do
    var <- Rv.globalVar (Txt.pack $ genId ident)
    regGlobVar ident var
  GlobFunDecl ident params body -> mdo
    regFun ident fun (length params)
    fun <- locally $ do
      Rv.function
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

genExpr :: Expression -> CodeGenM Rv.Operand
genExpr = \case
  ExprAtom atom -> genAtom atom
  ExprComp ce -> genComp ce
  ExprLetIn (ident, val) expr -> do
    val' <- genExpr val
    regLocVar ident val'
    genExpr expr

genAtom :: AtomicExpression -> CodeGenM Rv.Operand
genAtom = \case
  AtomId ident -> findAny ident
  AtomUnit -> Rv.immediate 0
  AtomBool bool -> Rv.immediate $ fromBool bool
  AtomInt int -> Rv.immediate int

genComp :: ComplexExpression -> CodeGenM Rv.Operand
genComp = \case
  CompApp f arg -> do
    f' <- findAny f
    arg' <- genAtom arg
    applyF <- findFun (Txt "miniml_apply")
    Rv.call applyF [f', arg']
  CompIte c t e -> do
    c' <- genAtom c
    Rv.ite c' (\_ -> genExpr t) (\_ -> genExpr e)
  CompBinOp op lhs rhs -> do
    lhs' <- genAtom lhs
    rhs' <- genAtom rhs
    let opF = case op of
          BoolOp AndOp -> Rv.and
          BoolOp OrOp -> Rv.or
          ArithOp PlusOp -> Rv.add
          ArithOp MinusOp -> Rv.sub
          ArithOp MulOp -> Rv.mul
          ArithOp DivOp ->
            ( \lhs'' rhs'' -> do
                divF <- findFun (Txt "miniml_div")
                Rv.call divF [lhs'', rhs'']
            )
          CompOp EqOp -> Rv.eq
          CompOp NeOp -> Rv.ne
          CompOp LtOp -> Rv.lt
          CompOp LeOp -> Rv.le
          CompOp GtOp -> Rv.gt
          CompOp GeOp -> Rv.ge
    opF lhs' rhs'
  CompUnOp op x -> do
    x' <- genAtom x
    let opF = case op of
          UnMinusOp -> Rv.neg
    opF x'

-- Vars & Funs

findAny :: Identifier' -> CodeGenM Rv.Operand
findAny ident = do
  maybeLocVar <- gets ((Map.!? ident) . locVars)
  case maybeLocVar of
    Just locVar -> return locVar
    Nothing -> do
      maybeFun <- gets ((Map.!? ident) . funs)
      case maybeFun of
        Just (fun, arity) -> do
          funToPafF <- findFun (Txt "miniml_fun_to_paf")
          arity' <- Rv.immediate $ fromIntegral arity
          Rv.call funToPafF [fun, arity']
        Nothing -> findGlobVar ident

findGlobVar :: (MonadState Env m) => Identifier' -> m Rv.Operand
findGlobVar ident = gets ((Map.! ident) . globVars)

findFun :: Identifier' -> CodeGenM Rv.Operand
findFun ident = gets (fst . (Map.! ident) . funs)

regLocVar :: (MonadState Env m) => Identifier' -> Rv.Operand -> m ()
regLocVar ident var = modify $
  \env -> env {locVars = Map.insert ident var (locVars env)}

regGlobVar :: (MonadState Env m) => Identifier' -> Rv.Operand -> m ()
regGlobVar ident gVar = modify $
  \env -> env {globVars = Map.insert ident gVar (globVars env)}

regFun :: (MonadState Env m) => Identifier' -> Rv.Operand -> Arity -> m ()
regFun ident fun paramsCnt = modify $
  \env -> env {funs = Map.insert ident (fun, paramsCnt) (funs env)}
