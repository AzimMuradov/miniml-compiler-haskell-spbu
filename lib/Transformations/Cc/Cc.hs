{-# LANGUAGE LambdaCase #-}

module Transformations.Cc.Cc (ccAst) where

import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import Data.Foldable (Foldable (foldl'))
import Data.Foldable.Extra (notNull)
import Data.Functor.Foldable (ListF (..), Recursive (cata))
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import MonadUtils
import qualified StdLib
import qualified Transformations.Simplifier.SimplifiedAst as Ast
import qualified Trees.Common as Ast

-- * AST Closure Converter

-- | Convert AST to its closure-free representation.
ccAst :: Ast.Program -> Ast.Program
ccAst (Ast.Program decls cnt) =
  let decls' = runReader (mapM ccGDecl decls) (Env gIds Map.empty)
      gIds = Set.fromList $ (Ast.Txt <$> StdLib.decls) <> (Ast.declId <$> decls)
   in Ast.Program decls' cnt

-- * Internal

-- ** Closure Converter Info

type CcInfo = Reader Env

data Env = Env
  { globalIds :: Set Ast.Identifier',
    declIdMappings :: Map Ast.Identifier' Ast.Expression
  }

-- ** Closure Converters

ccGDecl :: Ast.Declaration -> CcInfo Ast.Declaration
ccGDecl gDecl = case gDecl of
  Ast.DeclVar ident val -> cc1 (Ast.DeclVar ident) val
  Ast.DeclFun ident isRec (Ast.Fun params body) -> do
    fun <- cc1 (Ast.Fun params) body
    return $ Ast.DeclFun ident isRec fun

ccExpr :: Ast.Expression -> CcInfo Ast.Expression
ccExpr = \case
  Ast.ExprId ident -> do
    ms <- asks declIdMappings
    return $ fromMaybe (Ast.ExprId ident) (ms Map.!? ident)
  Ast.ExprPrimVal val -> return $ Ast.ExprPrimVal val
  Ast.ExprBinOp op lhs rhs -> cc2 (Ast.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> cc1 (Ast.ExprUnOp op) x
  Ast.ExprApp f arg -> cc2 Ast.ExprApp f arg
  Ast.ExprIte c t e -> cc3 Ast.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value -> do
      decl' <- cc1 (Ast.DeclVar ident) value
      cc1 (Ast.ExprLetIn decl') expr
    Ast.DeclFun ident isRec (Ast.Fun params body) -> do
      gs <- asks globalIds
      let fv = Set.toList $ findFv body \\ (toSet (ident <| params) <> gs)

      let withNewMappings =
            if notNull fv
              then local $ \env ->
                let app = apply (Ast.ExprId ident) (Ast.ExprId <$> fv)
                    ms = Map.insert ident app (declIdMappings env)
                 in env {declIdMappings = ms}
              else id

      withNewMappings $ do
        closedFun <- cc1 (Ast.Fun (prependList fv params)) body
        let decl' = Ast.DeclFun ident isRec closedFun
        cc1 (Ast.ExprLetIn decl') expr
  Ast.ExprFun (Ast.Fun params body) -> do
    gs <- asks globalIds
    let fv = Set.toList $ findFv body \\ (toSet params <> gs)

    closedFun <- cc1 (Ast.Fun (prependList fv params)) body
    return $ apply (Ast.ExprFun closedFun) (Ast.ExprId <$> fv)
  where
    apply :: Foldable t => Ast.Expression -> t Ast.Expression -> Ast.Expression
    apply = foldl' Ast.ExprApp

findFv :: Ast.Expression -> Set Ast.Identifier'
findFv = \case
  Ast.ExprId ident -> Set.singleton ident
  Ast.ExprPrimVal _ -> Set.empty
  Ast.ExprBinOp _ lhs rhs -> findFv' [lhs, rhs]
  Ast.ExprUnOp _ x -> findFv x
  Ast.ExprApp f arg -> findFv' [f, arg]
  Ast.ExprIte c t e -> findFv' [c, t, e]
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value ->
      let fv = findFv' [value, expr]
       in Set.delete ident fv
    Ast.DeclFun ident _ (Ast.Fun params body) ->
      let fv = findFv' [body, expr]
       in fv \\ toSet (ident <| params)
  Ast.ExprFun (Ast.Fun params body) -> findFv body \\ toSet params
  where
    findFv' :: [Ast.Expression] -> Set Ast.Identifier'
    findFv' = cata merger
      where
        merger (Cons x accum) = accum <> findFv x
        merger Nil = Set.empty

-- ** Collection Utils

toSet :: Ord a => NonEmpty a -> Set a
toSet = Set.fromList . NE.toList

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] ne = ne
prependList l ne = NE.fromList l <> ne

-- ** Utils

cc1 ::
  (Ast.Expression -> a) ->
  (Ast.Expression -> CcInfo a)
cc1 = liftM1' ccExpr

cc2 ::
  (Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> CcInfo a)
cc2 = liftM2' ccExpr

cc3 ::
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> CcInfo a)
cc3 = liftM3' ccExpr
