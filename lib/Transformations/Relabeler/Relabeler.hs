{-# LANGUAGE LambdaCase #-}

module Transformations.Relabeler.Relabeler (relabelAst) where

import Control.Monad (replicateM_)
import Control.Monad.State (State, gets, modify, runState)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import MonadUtils
import qualified Transformations.Simplifier.SimplifiedAst as Ast
import qualified Trees.Common as Ast

-- * AST Relabeler

-- | Relabel identifiers in the AST so that each declaration creates an identifier with a unique name.
-- It helps to avoid naming errors in the future.
relabelAst :: Ast.Program -> Ast.Program
relabelAst (Ast.Program decls cnt) =
  let (decls', Env _ cnt') = runState (mapM relabelDecl decls) (Env [] cnt)
   in Ast.Program decls' cnt'

-- * Internal

-- ** Relabeler State

type RelabelerState = State Env

data Env = Env
  { idMappings :: [IdMapping],
    idCnt :: Ast.IdCnt
  }

type IdMapping = (Ast.Identifier', Ast.Identifier')

-- ** Relabelers

relabelDecl :: Ast.Declaration -> RelabelerState Ast.Declaration
relabelDecl = \case
  Ast.DeclVar ident value -> do
    value' <- relabelExpr value
    ident' <- pushAndMapId ident
    return $ Ast.DeclVar ident' value'
  Ast.DeclFun ident isRec fun -> do
    (ident', fun') <-
      if isRec
        then (,) <$> pushAndMapId ident <*> relabelFun fun
        else flip (,) <$> relabelFun fun <*> pushAndMapId ident
    return $ Ast.DeclFun ident' isRec fun'

relabelExpr :: Ast.Expression -> RelabelerState Ast.Expression
relabelExpr = \case
  Ast.ExprId ident -> Ast.ExprId <$> mapId ident
  Ast.ExprVal val -> return $ Ast.ExprVal val
  Ast.ExprBinOp op lhs rhs -> relabel2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> relabel1 (Ast.ExprUnOp op) x
  Ast.ExprApp f args -> relabel2 Ast.ExprApp f args
  Ast.ExprIte c t e -> relabel3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> do
    decl' <- relabelDecl decl
    expr' <- relabelExpr expr
    popId
    return $ Ast.ExprLetIn decl' expr'
  Ast.ExprFun fun -> Ast.ExprFun <$> relabelFun fun

relabelFun :: Ast.Fun -> RelabelerState Ast.Fun
relabelFun (Ast.Fun params body) = do
  params' <- mapM pushAndMapId params
  body' <- relabelExpr body
  replicateM_ (NE.length params) popId
  return $ Ast.Fun params' body'

-- ** Identifier Mappings

mapId :: Ast.Identifier' -> RelabelerState Ast.Identifier'
mapId ident = do
  ms <- gets idMappings
  return $ fromMaybe ident (lookup ident ms)

pushAndMapId :: Ast.Identifier' -> RelabelerState Ast.Identifier'
pushAndMapId ident = pushId ident >> mapId ident

pushId :: Ast.Identifier' -> RelabelerState ()
pushId ident = modify $ \(Env ms cnt) ->
  Env
    { idMappings = (ident, Ast.Gen cnt (getName ident)) : ms,
      idCnt = cnt + 1
    }
  where
    getName (Ast.Gen _ name) = name
    getName (Ast.Txt name) = name

popId :: RelabelerState ()
popId = modify $ \env@(Env ms _) -> env {idMappings = tail ms}

-- ** Utils

relabel1 ::
  (Ast.Expression -> a) ->
  (Ast.Expression -> RelabelerState a)
relabel1 = liftM1' relabelExpr

relabel2 ::
  (Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> RelabelerState a)
relabel2 = liftM2' relabelExpr

relabel3 ::
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> RelabelerState a)
relabel3 = liftM3' relabelExpr
