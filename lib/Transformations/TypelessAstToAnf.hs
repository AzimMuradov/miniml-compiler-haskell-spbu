module Transformations.TypelessAstToAnf where

import Control.Monad.Cont (ContT, evalContT, mapContT)
import Control.Monad.State (MonadTrans (lift), State, evalState, get, modify)
import Data.Text (Text, cons, pack)
import qualified Transformations.Anf as Anf
import Transformations.TypelessAst

transformTypelessAstToAnf :: Program -> Anf.Program
transformTypelessAstToAnf = runANormal . normalizeProgram

type AnfRes r a = ContT r (State Int) a

runANormal :: AnfRes a a -> a
runANormal x = evalState (evalContT x) 0

normalizeProgram :: Program -> AnfRes r Anf.Program
normalizeProgram (Program stmts) = Anf.Program <$> mapM normalizeDefine stmts

normalizeDefine :: Statement -> AnfRes r Anf.Statement
normalizeDefine (StmtDecl name value) = Anf.StmtDecl name <$> normalizeTerm value
normalizeDefine (StmtExpr expr) = Anf.StmtExpr <$> normalizeTerm expr

normalizeExpr :: Expression -> AnfRes Anf.Expression Anf.Expression
normalizeExpr (ExprIdentifier name) = return $ Anf.ExprAtomExpr $ Anf.AtomExprIdentifier name
normalizeExpr (ExprValue l) = case l of
  ValUnit -> return $ Anf.ExprAtomExpr Anf.AtomExprUnit
  ValBool bool -> return $ Anf.ExprAtomExpr $ Anf.AtomExprBool bool
  ValInt int -> return $ Anf.ExprAtomExpr $ Anf.AtomExprInt int
  ValFun args body -> Anf.ExprAtomExpr <$> (Anf.AtomExprClosure args <$> normalizeTerm body)
normalizeExpr (ExprApplication a b) = Anf.ExprCompExpr <$> (Anf.CompExprApp <$> normalizeName a <*> normalizeName b)
normalizeExpr (ExprIte c t e) = do
  c' <- normalizeName c
  t' <- normalizeTerm t
  e' <- normalizeTerm e
  return $ Anf.ExprCompExpr $ Anf.CompExprIte c' t' e'
normalizeExpr (ExprLetIn x value expr) = Anf.ExprLetIn x <$> normalizeExpr value <*> normalizeExpr expr

normalizeName :: Expression -> AnfRes Anf.Expression Anf.AtomicExpression
normalizeName expr = do
  expr' <- normalizeExpr expr
  case expr' of
    Anf.ExprAtomExpr atom -> return atom
    _ -> do
      name <- genName
      mapContT
        (\expr'' -> Anf.ExprLetIn name expr' <$> expr'')
        (return $ Anf.AtomExprIdentifier name)

normalizeTerm :: Expression -> AnfRes r Anf.Expression
normalizeTerm expr = lift $ evalContT $ normalizeExpr expr

genName :: AnfRes r Text
genName = do
  n <- get
  modify (+ 1)
  return $ cons '$' (pack $ show n)
