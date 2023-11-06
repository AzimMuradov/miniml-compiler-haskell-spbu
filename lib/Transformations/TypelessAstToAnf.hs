module Transformations.TypelessAstToAnf (typelessAstToAnf) where

import Control.Monad.Cont (ContT, evalContT, mapContT)
import Control.Monad.State (MonadTrans (lift), State, evalState, get, modify)
import Data.Text (Text, cons, pack)
import qualified Transformations.Anf as Anf
import Transformations.RelabelVars
import qualified Transformations.TypelessAst as TAst

typelessAstToAnf :: TAst.Program -> Anf.Program
typelessAstToAnf program = evalState (normalizeProgram relabeledProgram) cnt
  where
    (relabeledProgram, Env _ cnt) = relabelTypelessAst program

type CpsWithCnt r a = ContT r CntState a

type CntState = State Int

normalizeProgram :: TAst.Program -> CntState Anf.Program
normalizeProgram (TAst.Program stmts) = Anf.Program <$> mapM normalizeStatement stmts

normalizeStatement :: TAst.Statement -> CntState Anf.Statement
normalizeStatement (TAst.StmtDecl name value _) = Anf.StmtDecl name <$> normalizeExpr value
normalizeStatement (TAst.StmtExpr expr) = Anf.StmtExpr <$> normalizeExpr expr

normalizeExpr :: TAst.Expression -> CntState Anf.Expression
normalizeExpr (TAst.ExprIdentifier name) = returnAtom $ Anf.AtomIdentifier name
normalizeExpr (TAst.ExprValue value) = case value of
  TAst.ValUnit -> returnAtom Anf.AtomUnit
  TAst.ValBool bool -> returnAtom $ Anf.AtomBool bool
  TAst.ValInt int -> returnAtom $ Anf.AtomInt int
  TAst.ValFun params body -> Anf.ExprAtom . Anf.AtomClosure params <$> normalizeExpr body
normalizeExpr (TAst.ExprApplication f args) = evalContT $ do
  f' <- normalizeName f
  args' <- mapM normalizeName args
  returnComplex $ Anf.CompApp f' args'
normalizeExpr (TAst.ExprIte c t e) = evalContT $ do
  c' <- normalizeName c
  t' <- lift $ normalizeExpr t
  e' <- lift $ normalizeExpr e
  returnComplex $ Anf.CompIte c' t' e'
normalizeExpr (TAst.ExprLetIn (name, value, _) expr) = do
  value' <- normalizeExpr value
  expr' <- normalizeExpr expr
  return $ Anf.ExprLetIn name value' expr'

returnAtom :: Anf.AtomicExpression -> CntState Anf.Expression
returnAtom = return . Anf.ExprAtom

returnComplex :: Anf.ComplexExpression -> CpsWithCnt Anf.Expression Anf.Expression
returnComplex = return . Anf.ExprComp

normalizeName :: TAst.Expression -> CpsWithCnt Anf.Expression Anf.AtomicExpression
normalizeName expr = do
  expr' <- lift $ normalizeExpr expr
  case expr' of
    Anf.ExprAtom atom -> return atom
    _ -> do
      name <- lift genName
      mapContT
        (\e -> Anf.ExprLetIn name expr' <$> e)
        (return $ Anf.AtomIdentifier name)

genName :: CntState Text
genName = do
  n <- get
  modify (+ 1)
  return $ cons '$' (pack $ show n)
