module Transformations.RelabelVars
  ( RelabelerState,
    Env (..),
    Scope,
    relabelTypelessAst,
  )
where

import Control.Monad (replicateM_)
import Control.Monad.State (MonadState (get), State, modify, runState)
import Data.Maybe (fromMaybe)
import Data.Text (cons, pack)
import Transformations.TypelessAst

-- Identifier relabeling to avoid naming errors

type RelabelerState = State Env

data Env = Env
  { _scopes :: [Scope],
    _cnt :: Int
  }

emptyEnv :: Env
emptyEnv = Env [] 0

type Scope = (Identifier, Identifier)

relabelTypelessAst :: Program -> (Program, Env)
relabelTypelessAst program = runState (relabelTypelessAst' program) emptyEnv

relabelTypelessAst' :: Program -> RelabelerState Program
relabelTypelessAst' (Program stmts) = Program <$> mapM relabelStatement stmts

relabelStatement :: Statement -> RelabelerState Statement
relabelStatement stmt = case stmt of
  StmtDecl name expr isRec -> do
    (name', expr') <-
      if isRec
        then do
          pushScope name
          name' <- findLabel name
          expr' <- relabelExpression expr
          return (name', expr')
        else do
          expr' <- relabelExpression expr
          pushScope name
          name' <- findLabel name
          return (name', expr')
    return $ StmtDecl name' expr' isRec
  StmtExpr expr -> StmtExpr <$> relabelExpression expr

relabelExpression :: Expression -> RelabelerState Expression
relabelExpression (ExprIdentifier name) = ExprIdentifier <$> findLabel name
relabelExpression (ExprValue value) = do
  value' <- case value of
    ValFun params body -> do
      mapM_ pushScope params
      params' <- mapM findLabel params
      body' <- relabelExpression body
      replicateM_ (length params) popScope
      return $ ValFun params' body'
    _ -> return value
  return $ ExprValue value'
relabelExpression (ExprApplication f args) = do
  f' <- relabelExpression f
  args' <- mapM relabelExpression args
  return $ ExprApplication f' args'
relabelExpression (ExprIte c t e) = do
  c' <- relabelExpression c
  t' <- relabelExpression t
  e' <- relabelExpression e
  return $ ExprIte c' t' e'
relabelExpression (ExprLetIn (name, value, isRec) expr) = do
  (name', value') <-
    if isRec
      then do
        pushScope name
        name' <- findLabel name
        value' <- relabelExpression value
        return (name', value')
      else do
        value' <- relabelExpression value
        pushScope name
        name' <- findLabel name
        return (name', value')
  expr' <- relabelExpression expr
  popScope
  return $ ExprLetIn (name', value', isRec) expr'

findLabel :: Identifier -> RelabelerState Identifier
findLabel name = do
  Env scs _ <- get
  return $ fromMaybe name (lookup name scs)

pushScope :: Identifier -> RelabelerState ()
pushScope name = modify $ \(Env scs cnt) ->
  Env
    { _scopes = (name, cons '$' (pack $ show cnt)) : scs,
      _cnt = cnt + 1
    }

popScope :: RelabelerState ()
popScope = modify $ \env@(Env scs _) -> env {_scopes = tail scs}
