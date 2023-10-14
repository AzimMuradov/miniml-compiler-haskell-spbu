{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.Runtime where

import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification.IntVar (evalIntBindingT)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as M
import Parser.Ast (Statement)
import TypeInference.HindleyMilner
  ( Infer,
    Polytype,
    TypeError,
    UType,
    applyBindings,
    fromUPolytype,
    generalize,
  )
import TypeInference.TIRealization (inferStatement)

runInfer :: Infer UType -> Either TypeError Polytype
runInfer =
  (>>= applyBindings)
    >>> (>>= (generalize >>> fmap fromUPolytype))
    >>> flip runReaderT M.empty
    >>> runExceptT
    >>> evalIntBindingT
    >>> runIdentity

inferPolytype :: [Statement] -> Either TypeError Polytype
inferPolytype = runInfer . inferStatement
