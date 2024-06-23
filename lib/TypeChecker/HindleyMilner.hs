{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeChecker.HindleyMilner
  ( Infer,
    TypeError (..),
    UType,
    Polytype,
    applyBindings,
    generalize,
    toPolytype,
    toUType,
    withBinding,
    fresh,
    Poly (..),
    UTerm (UTVar, UTUnit, UTBool, UTInt, UTFun),
    (=:=),
    lookup,
    TypeF (..),
    mkVarName,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification hiding (applyBindings, (=:=))
import qualified Control.Unification as U
import Control.Unification.IntVar
import Data.Foldable (fold)
import Data.Functor.Fixedpoint
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Text (pack)
import GHC.Generics (Generic1)
import qualified Trees.Common as L -- Lang
import Prelude hiding (lookup)

-- * Type

type Type = Fix TypeF

data TypeF a
  = TVarF L.Identifier
  | TUnitF
  | TBoolF
  | TIntF
  | TFunF a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

-- * UType

type UType = UTerm TypeF IntVar

pattern UTVar :: L.Identifier -> UType
pattern UTVar var = UTerm (TVarF var)

pattern UTUnit :: UType
pattern UTUnit = UTerm TUnitF

pattern UTBool :: UType
pattern UTBool = UTerm TBoolF

pattern UTInt :: UType
pattern UTInt = UTerm TIntF

pattern UTFun :: UType -> UType -> UType
pattern UTFun funT argT = UTerm (TFunF funT argT)

-- * Polytype

data Poly t = Forall [L.Identifier] t
  deriving (Eq, Show, Functor)

type Polytype = Poly Type

type UPolytype = Poly UType

-- * Converters

toUType :: L.Type -> UType
toUType = \case
  L.TUnit -> UTUnit
  L.TBool -> UTBool
  L.TInt -> UTInt
  L.TFun funT argT -> UTFun (toUType funT) (toUType argT)

toPolytype :: UPolytype -> Polytype
toPolytype = fmap (fromJust . freeze)

-- * Infer

type Infer = ReaderT Ctx (ExceptT TypeError (IntBindingT TypeF Identity))

type Ctx = Map L.Identifier UPolytype

lookup :: L.Identifier -> Infer UType
lookup var = do
  varUPT <- asks $ M.lookup var
  maybe (throwError $ UnboundVar var) instantiate varUPT
  where
    instantiate :: UPolytype -> Infer UType
    instantiate (Forall xs uty) = do
      xs' <- mapM (const fresh) xs
      return $ substU (M.fromList (zip (map Left xs) xs')) uty

withBinding :: (MonadReader Ctx m) => L.Identifier -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

ucata :: (Functor t) => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

deriving instance Ord IntVar

-- * FreeVars

class FreeVars a where
  freeVars :: a -> Infer (Set (Either L.Identifier IntVar))

instance FreeVars UType where
  freeVars ut = do
    fuvs <- fmap (S.fromList . map Right) . lift . lift $ getFreeVars ut
    let ftvs =
          ucata
            (const S.empty)
            (\case TVarF x -> S.singleton (Left x); f -> fold f)
            ut
    return $ fuvs `S.union` ftvs

instance FreeVars UPolytype where
  freeVars (Forall xs ut) = (\\ S.fromList (map Left xs)) <$> freeVars ut

instance FreeVars Ctx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

-- * Errors

data TypeError where
  Unreachable :: TypeError
  UnboundVar :: L.Identifier -> TypeError
  Infinite :: IntVar -> UType -> TypeError
  ImpossibleBinOpApplication :: UType -> UType -> TypeError
  ImpossibleUnOpApplication :: UType -> TypeError
  Mismatch :: TypeF UType -> TypeF UType -> TypeError
  deriving (Show)

instance Fallible TypeF IntVar TypeError where
  occursFailure = Infinite
  mismatchFailure = Mismatch

(=:=) :: UType -> UType -> Infer UType
s =:= t = lift $ s U.=:= t

applyBindings :: UType -> Infer UType
applyBindings = lift . U.applyBindings

substU :: Map (Either L.Identifier IntVar) UType -> UType -> UType
substU m =
  ucata
    (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
    ( \case
        TVarF v -> fromMaybe (UTVar v) (M.lookup (Left v) m)
        f -> UTerm f
    )

mkVarName :: String -> IntVar -> L.Identifier
mkVarName nm (IntVar v) = pack (nm <> show (v + (maxBound :: Int) + 1))

generalize :: UType -> Infer UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmFreeVars <- freeVars uty'
  ctxFreeVars <- freeVars ctx
  let fvs = S.toList $ tmFreeVars \\ ctxFreeVars
      xs = map (either id (mkVarName "a")) fvs
  return $ Forall xs (substU (M.fromList (zip fvs (map UTVar xs))) uty')
