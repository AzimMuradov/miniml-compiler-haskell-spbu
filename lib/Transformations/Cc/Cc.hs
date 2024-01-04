{-# LANGUAGE LambdaCase #-}

module Transformations.Cc.Cc (ccAst) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), Reader, runReader)
import Control.Monad.State (StateT, evalStateT, get, modify)
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

newtype Env = Env {idMapping :: Map Ast.Identifier' Ast.Expression}

type GlobalsInfo = Reader GlobalIds

type GlobalIds = Set Ast.Identifier'

-- ** Closure Converters

ccGDecl :: Ast.Declaration -> GlobalsInfo Ast.Declaration
ccGDecl gDecl = flip evalStateT (Env Map.empty) $
  case gDecl of
    Ast.DeclVar ident val -> cc1 (Ast.DeclVar ident) val
    Ast.DeclFun ident isRec (Ast.Fun params body) -> do
      fun <- cc1 (Ast.Fun params) body
      return $ Ast.DeclFun ident isRec fun

ccExpr :: Ast.Expression -> CcState Ast.Expression
ccExpr = \case
  Ast.ExprId ident -> do
    Env m <- get
    return $ case m Map.!? ident of
      Just app -> app
      Nothing -> Ast.ExprId ident
  Ast.ExprPrimVal val -> return $ Ast.ExprPrimVal val
  Ast.ExprBinOp op lhs rhs -> cc2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> cc1 (Ast.ExprUnOp op) x
  Ast.ExprApp f arg -> cc2 Ast.ExprApp f arg
  Ast.ExprIte c t e -> cc3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value ->
      Ast.ExprLetIn <$> (Ast.DeclVar ident <$> ccExpr value) <*> ccExpr expr
    Ast.DeclFun ident isRec (Ast.Fun params body) -> do
      fvSet' <- lift $ findFv body
      let fvSet = fvSet' Set.\\ toSet (ident <| params)

      unless (null fvSet) $ do
        let fv = toNonEmpty fvSet
        modify $ \env ->
          let app = apply (Ast.ExprId ident) (Ast.ExprId <$> fv)
              m = Map.insert ident app (idMapping env)
           in env {idMapping = m}

      body' <- ccExpr body
      let decl' = Ast.DeclFun ident isRec (Ast.Fun (prependList (Set.toList fvSet) params) body')
      Ast.ExprLetIn decl' <$> ccExpr expr
  Ast.ExprFun (Ast.Fun params body) -> do
    fvSet <- lift $ findFv body

    let fv = Set.toList (fvSet Set.\\ toSet params)
    let params' = prependList fv params
    body' <- ccExpr body
    let closedFun = Ast.ExprFun (Ast.Fun params' body')

    return $ apply closedFun (Ast.ExprId <$> fv)
  where
    apply :: Foldable t => Ast.Expression -> t Ast.Expression -> Ast.Expression
    apply = foldl' Ast.ExprApp

findFv :: Ast.Expression -> GlobalsInfo (Set Ast.Identifier')
findFv = \case
  Ast.ExprId ident -> do
    gs <- ask
    return $
      if ident `Set.notMember` gs
        then Set.singleton ident
        else Set.empty
  Ast.ExprPrimVal _ -> return Set.empty
  Ast.ExprBinOp _ lhs rhs -> Set.union <$> findFv lhs <*> findFv rhs
  Ast.ExprUnOp _ x -> findFv x
  Ast.ExprApp f arg -> do
    f' <- findFv f
    arg' <- findFv arg
    return $ Set.union f' arg'
  Ast.ExprIte c t e -> do
    c' <- findFv c
    t' <- findFv t
    e' <- findFv e
    return $ Set.union c' (Set.union t' e')
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value -> do
      value' <- findFv value
      expr' <- findFv expr
      return $ Set.delete ident (Set.union value' expr')
    Ast.DeclFun ident _ (Ast.Fun params body) -> do
      body' <- findFv body
      expr' <- findFv expr
      return $ Set.union body' expr' Set.\\ toSet (ident <| params)
  Ast.ExprFun (Ast.Fun params body) -> do
    body' <- findFv body
    return $ body' Set.\\ toSet params

-- ** Collection Utils

toSet :: Ord a => NonEmpty a -> Set a
toSet = Set.fromList . NE.toList

toNonEmpty :: Set a -> NonEmpty a
toNonEmpty = NE.fromList . Set.toList

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] b = b
prependList a b = NE.fromList a <> b

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
