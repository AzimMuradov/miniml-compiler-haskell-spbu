{-# LANGUAGE LambdaCase #-}

module Transformations.Relabeler.RelabelVars (relabelAst) where

import Control.Monad (replicateM_)
import Control.Monad.State (MonadState (get), State, modify, runState)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Transformations.Simplification.SimplifiedAst as Ast
import qualified Trees.Common as Ast
import Utils

-- Identifier relabeling to avoid naming errors

relabelAst :: Ast.Program -> Ast.Program
relabelAst (Ast.Program tlDecls cnt) =
  let (tlDecls', Env _ cnt') = runState (mapM relabelDecl tlDecls) (Env [] cnt)
   in Ast.Program tlDecls' cnt'

-- Implementation

type RelabelerState = State Env

data Env = Env {scopes :: [Scope], idCnt :: Ast.IdCnt}

type Scope = (Ast.Identifier', Ast.Identifier')

relabelDecl :: Ast.Declaration -> RelabelerState Ast.Declaration
relabelDecl = \case
  Ast.DeclVar name value -> do
    value' <- relabelExpr value
    pushScope name
    name' <- findLabel name
    return $ Ast.DeclVar name' value'
  Ast.DeclFun name isRec fun -> do
    (name', fun') <-
      if isRec
        then do
          pushScope name
          name' <- findLabel name
          fun' <- relabelFun fun
          return (name', fun')
        else do
          fun' <- relabelFun fun
          pushScope name
          name' <- findLabel name
          return (name', fun')

    return $ Ast.DeclFun name' isRec fun'

relabelExpr :: Ast.Expression -> RelabelerState Ast.Expression
relabelExpr = \case
  Ast.ExprId name -> Ast.ExprId <$> findLabel name
  Ast.ExprVal val -> return $ Ast.ExprVal val
  Ast.ExprBinOp op lhs rhs -> relabel2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> relabel1 (Ast.ExprUnOp op) x
  Ast.ExprApp f args -> relabel2 Ast.ExprApp f args
  Ast.ExprIte c t e -> relabel3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> do
    decl' <- relabelDecl decl
    expr' <- relabelExpr expr

    popScope

    return $ Ast.ExprLetIn decl' expr'
  Ast.ExprFun fun -> Ast.ExprFun <$> relabelFun fun

relabelFun :: Ast.Fun -> RelabelerState Ast.Fun
relabelFun (Ast.Fun params body) = do
  -- TODO : same name in params
  mapM_ pushScope params
  params' <- mapM findLabel params

  body' <- relabelExpr body

  replicateM_ (NE.length params) popScope

  return $ Ast.Fun params' body'

findLabel :: Ast.Identifier' -> RelabelerState Ast.Identifier'
findLabel name = do
  Env scs _ <- get
  return $ fromMaybe name (lookup name scs)

pushScope :: Ast.Identifier' -> RelabelerState ()
pushScope name = modify $ \(Env scs cnt) ->
  Env
    { scopes = (name, Ast.Gen cnt) : scs,
      idCnt = cnt + 1
    }

popScope :: RelabelerState ()
popScope = modify $ \env@(Env scs _) -> env {scopes = tail scs}

-- Utils

relabel1 :: (Ast.Expression -> a) -> Ast.Expression -> RelabelerState a
relabel1 f x = f <$> relabelExpr x

relabel2 ::
  (Ast.Expression -> Ast.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  RelabelerState a
relabel2 = liftM2' relabelExpr

relabel3 ::
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  Ast.Expression ->
  RelabelerState a
relabel3 = liftM3' relabelExpr
