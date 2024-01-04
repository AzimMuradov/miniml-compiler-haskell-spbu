{-# OPTIONS_GHC -Wno-partial-fields #-}

module CodeGen.RunResult where

import CodeGen.TimedValue (Nanoseconds)
import Data.Text (Text)

data RunResult
  = Success
      { stdout :: Text,
        compTime :: Nanoseconds,
        runTime :: Nanoseconds
      }
  | CompilationError
      { compErrMsg :: Text,
        compTime :: Nanoseconds
      }
  | RuntimeError
      { stdout :: Text,
        stderr :: Text,
        exitCode :: Int,
        compTime :: Nanoseconds,
        runTime :: Nanoseconds
      }
  deriving (Show)
