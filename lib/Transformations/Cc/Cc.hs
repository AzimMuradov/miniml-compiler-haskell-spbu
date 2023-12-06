{-# LANGUAGE LambdaCase #-}

module Transformations.Cc.Cc (ccAst) where

import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Transformations.Simplification.SimplifiedAst as Ast
import qualified Trees.Common as Ast
import Utils

-- | Convert AST to Closure-Free Representation
ccAst :: Ast.Program -> Ast.Program
ccAst prg = evalState (ccProgram prg) initEnv
  where
    initEnv = Env {freeVars = Set.empty, globals = getGlobals prg}

    -- TODO : Add stdlib globals

    getGlobals (Ast.Program decls _) = Set.fromList $ getDeclId <$> decls

    getDeclId (Ast.DeclVar ident _) = ident
    getDeclId (Ast.DeclFun ident _ _) = ident

-- let f p1 p2 p3 = body[p1 p2 p3] in expr[p1 p2 p3 f]
-- \a -> \b -> a + b ==> \a -> (\a b -> a + b) a
-- let f x y a b = body in expr

-- Implementation

data Env = Env
  { freeVars :: Set Ast.Identifier',
    globals :: Set Ast.Identifier'
    -- TODO : map :: Map Ast.Identifier' Ast.Expression
  }

type CcState = State Env

ccProgram :: Ast.Program -> CcState Ast.Program
ccProgram (Ast.Program gs cnt) = flip Ast.Program cnt <$> mapM ccTopLevelDecl gs
  where
    ccTopLevelDecl decl = ccTopLevelDecl' decl <* clearFv

    ccTopLevelDecl' = \case
      Ast.DeclVar name expr ->
        cc1 (Ast.DeclVar name) expr
      Ast.DeclFun name isRec (Ast.Fun params body) ->
        cc1 (Ast.DeclFun name isRec . Ast.Fun params) body

    clearFv = modify $ \env -> env {freeVars = Set.empty}

ccExpr :: Ast.Expression -> CcState Ast.Expression
ccExpr = \case
  Ast.ExprId name -> do
    modify $ \env@(Env fv gs) ->
      let newFV = if name `Set.notMember` gs then Set.singleton name else Set.empty
       in env {freeVars = fv `Set.union` newFV}
    return $ Ast.ExprId name
  Ast.ExprVal val -> return $ Ast.ExprVal val
  Ast.ExprBinOp op lhs rhs -> cc2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> cc1 (Ast.ExprUnOp op) x
  Ast.ExprApp f arg -> cc2 Ast.ExprApp f arg
  Ast.ExprIte c t e -> cc3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar name value ->
      Ast.ExprLetIn <$> (Ast.DeclVar name <$> ccExpr value) <*> ccExpr expr
    Ast.DeclFun name isRec (Ast.Fun params body) -> do
      body' <- ccExpr body

      fv <- do
        Env fvSet _ <- get
        return $ toNonEmpty $ fvSet Set.\\ toSet (name <| params)

      let decl' = Ast.DeclFun name isRec (Ast.Fun (fv <> params) body')
      let expr' = foldl' Ast.ExprApp expr (Ast.ExprId <$> fv)

      return $ Ast.ExprLetIn decl' expr'
  Ast.ExprFun (Ast.Fun params body) -> ccFun params body

ccFun :: NonEmpty Ast.Identifier' -> Ast.Expression -> CcState Ast.Expression
ccFun params body = do
  body' <- ccExpr body

  fvSet <- do
    Env fvSet _ <- get
    return $ fvSet Set.\\ toSet params

  let args' = prependList (Set.toList fvSet) params
  let closedFun = Ast.ExprFun (Ast.Fun args' body')
  let argsExprs = Ast.ExprId <$> args'

  modify $ \env -> env {freeVars = fvSet}

  return $
    if null fvSet
      then closedFun
      else foldl Ast.ExprApp closedFun argsExprs

-- Utils

toSet :: Ord a => NonEmpty a -> Set a
toSet = Set.fromList . NE.toList

toNonEmpty :: Set a -> NonEmpty a
toNonEmpty = NE.fromList . Set.toList

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList = (<>) . NE.fromList

cc1 :: (Ast.Expression -> a) -> Ast.Expression -> CcState a
cc1 f x = f <$> ccExpr x

cc2 ::
  (Ast.Expression -> Ast.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  CcState a
cc2 = liftM2' ccExpr

cc3 ::
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  Ast.Expression ->
  CcState a
cc3 = liftM3' ccExpr
