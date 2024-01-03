{-# LANGUAGE LambdaCase #-}

module Transformations.Cc.Cc (ccAst) where

import Control.Monad (void)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, when)
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MonadUtils
import qualified StdLib
import qualified Transformations.Simplifier.SimplifiedAst as Ast
import qualified Trees.Common as Ast

-- * AST Closure Converter

-- | Convert AST to its closure-free representation.
ccAst :: Ast.Program -> Ast.Program
ccAst (Ast.Program decls cnt) =
  let decls' = runReader (mapM ccGDecl decls) gIds
      gIds = Set.fromList $ (Ast.Txt <$> StdLib.decls) <> (Ast.declId <$> decls)
   in Ast.Program decls' cnt

-- * Internal

-- ** Closure Converter State & Globals Info

type CcState = StateT Env GlobalsInfo

data Env = Env
  { freeVars :: Set Ast.Identifier',
    idMapping :: Map Ast.Identifier' Ast.Expression
  }

type GlobalsInfo = Reader GlobalIds

type GlobalIds = Set Ast.Identifier'

-- ** Closure Converters

ccGDecl :: Ast.Declaration -> GlobalsInfo Ast.Declaration
ccGDecl gDecl = flip evalStateT (Env Set.empty Map.empty) $
  case gDecl of
    Ast.DeclVar ident val -> cc1 (Ast.DeclVar ident) val
    Ast.DeclFun ident isRec (Ast.Fun params body) -> do
      fun <- cc1 (Ast.Fun params) body
      return $ Ast.DeclFun ident isRec fun

ccExpr :: Ast.Expression -> CcState Ast.Expression
ccExpr = \case
  Ast.ExprId ident -> do
    gs <- ask
    Env fv m <- get
    case m Map.!? ident of
      Just app -> return app
      Nothing -> do
        when (ident `Set.notMember` gs) $
          modify $ \env -> env {freeVars = ident `Set.insert` fv}
        return $ Ast.ExprId ident
  Ast.ExprPrimVal val -> return $ Ast.ExprPrimVal val
  Ast.ExprBinOp op lhs rhs -> cc2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> cc1 (Ast.ExprUnOp op) x
  Ast.ExprApp f arg -> cc2 Ast.ExprApp f arg
  Ast.ExprIte c t e -> cc3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value ->
      Ast.ExprLetIn <$> (Ast.DeclVar ident <$> ccExpr value) <*> ccExpr expr
    Ast.DeclFun ident isRec (Ast.Fun params body) -> do
      -- Find all free variables in body.
      void $ ccExpr body
      fv <- gets $ \env ->
        let fvSet = freeVars env Set.\\ toSet (ident <| params)
         in toNonEmpty fvSet
      modify $ \env ->
        let app = apply (Ast.ExprId ident) (Ast.ExprId <$> fv)
            m = Map.insert ident app (idMapping env)
         in env {idMapping = m}

      body' <- ccExpr body
      let decl' = Ast.DeclFun ident isRec (Ast.Fun (fv <> params) body')
      Ast.ExprLetIn decl' <$> ccExpr expr
  Ast.ExprFun (Ast.Fun params body) -> do
    body' <- ccExpr body

    fvSet <- gets $ \env -> freeVars env Set.\\ toSet params
    modify $ \env -> env {freeVars = fvSet}

    let fv = Set.toList fvSet
    let params' = prependList fv params
    let closedFun = Ast.ExprFun (Ast.Fun params' body')

    return $ apply closedFun (Ast.ExprId <$> fv)
  where
    apply :: Foldable t => Ast.Expression -> t Ast.Expression -> Ast.Expression
    apply = foldl' Ast.ExprApp

-- ** Collection Utils

toSet :: Ord a => NonEmpty a -> Set a
toSet = Set.fromList . NE.toList

toNonEmpty :: Set a -> NonEmpty a
toNonEmpty = NE.fromList . Set.toList

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList = (<>) . NE.fromList

-- ** Utils

cc1 ::
  (Ast.Expression -> a) ->
  (Ast.Expression -> CcState a)
cc1 = liftM1' ccExpr

cc2 ::
  (Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> CcState a)
cc2 = liftM2' ccExpr

cc3 ::
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> CcState a)
cc3 = liftM3' ccExpr
