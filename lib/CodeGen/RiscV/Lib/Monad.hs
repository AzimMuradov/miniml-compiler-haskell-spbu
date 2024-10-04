{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeGen.RiscV.Lib.Monad where

import CodeGen.RiscV.Lib.Types (CodeLine)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..), get, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Int (Int64)

newtype AsmBuilderT m a = AsmBuilderT {unAsmBuilderT :: StateT BuilderState m a}
  deriving (Functor, Applicative, Monad, MonadFix) via StateT BuilderState m

data BuilderState = BS
  { programBS :: ProgramBuilderState,
    functionBS :: FunctionBuilderState,
    idCnt :: Integer
  }

data ProgramBuilderState = PBS
  { sectionText :: [[CodeLine]],
    sectionData :: [CodeLine]
  }

data FunctionBuilderState = FBS
  { functionCodeLines :: [[CodeLine]],
    stackPointerOffset :: Int64 -- In double words
  }

emptyBS :: BuilderState
emptyBS = BS emptyPBS emptyFBS 0

emptyPBS :: ProgramBuilderState
emptyPBS = PBS [] []

emptyFBS :: FunctionBuilderState
emptyFBS = FBS [] 0

instance (MonadState s m) => MonadState s (AsmBuilderT m) where
  state = lift . state

instance MonadTrans AsmBuilderT where
  lift = AsmBuilderT . lift

type AsmBuilder = AsmBuilderT Identity

class (Monad m) => MonadAsmBuilder m where
  getAsmBuilderState :: m BuilderState

  modifyAsmBuilderState :: (BuilderState -> BuilderState) -> m ()

  default getAsmBuilderState ::
    (MonadTrans t, MonadAsmBuilder m1, m ~ t m1) =>
    m BuilderState
  getAsmBuilderState = lift getAsmBuilderState

  default modifyAsmBuilderState ::
    (MonadTrans t, MonadAsmBuilder m1, m ~ t m1) =>
    (BuilderState -> BuilderState) ->
    m ()
  modifyAsmBuilderState = lift . modifyAsmBuilderState

instance (Monad m) => MonadAsmBuilder (AsmBuilderT m) where
  getAsmBuilderState = AsmBuilderT get

  modifyAsmBuilderState = AsmBuilderT . modify

instance (MonadAsmBuilder m) => MonadAsmBuilder (StateT s m)
