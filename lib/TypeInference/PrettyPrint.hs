{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.PrettyPrint where

import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification.IntVar
import Data.Functor.Fixedpoint
import Data.Text (unpack)
import Text.Printf
import TypeInference.HindleyMilner
import Prelude hiding (lookup)

type Prec = Int

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0

  prettyPrec :: Prec -> p -> String
  prettyPrec _ = pretty

instance (Pretty (t (Fix t))) => Pretty (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (Pretty t) => Pretty (HType t) where
  prettyPrec _ (TyVarF x) = unpack x
  prettyPrec _ TyBoolF = "bool"
  prettyPrec _ TyIntF = "int"
  prettyPrec p (TyFunF ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 ty1 ++ " -> " ++ prettyPrec 0 ty2

instance (Pretty (t (UTerm t v)), Pretty v) => Pretty (UTerm t v) where
  pretty (UTerm t) = pretty t
  pretty (UVar v) = pretty v

instance Pretty Polytype where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = unwords ("forall" : (unpack <$> xs)) ++ ". " ++ pretty t

mparens :: Bool -> String -> String
mparens True = ("(" ++) . (++ ")")
mparens False = id

instance Pretty IntVar where
  pretty = unpack . mkVarName "u"

instance Pretty TypeError where
  pretty EmptyList = printf "List of statemnet is empty"
  pretty Unreachable = printf "Unreachable state"
  pretty (ImpossibleOpApplication c1 c2) = printf "It is not possible to apply this operation between '%s' and '%s'" (prettyPrec 0 c1) (prettyPrec 0 c2)
  pretty (DuplicateDifinition x) = printf "Duplicate definition of value '%s'" (unpack x)
  pretty (UnboundVar x) = printf "Unbound variable '%s'" (unpack x)
  pretty (Infinite x ty) = printf "Infinite type %s = %s" (pretty x) (pretty ty)
  pretty (Mismatch ty1 ty2) = printf "The type '%s' does not match the type '%s'" (pphelper $ pretty ty1) (pphelper $ pretty ty2)
    where
      pphelper s = if s /= "" then s else "non measure"
