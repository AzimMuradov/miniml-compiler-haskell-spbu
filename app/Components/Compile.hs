{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Compile (compile) where

import Configuration.AppConfiguration (CompilationTarget (..), Compile (..), Debug, Input (..))
import Data.Text (Text)
import qualified Data.Text as Txt

compile :: Compile -> Debug -> IO ()
compile (Compile input target output) debug = do
  text <- readText input

  case target of
    CompileToLlvmIr -> error "Not Yet Implemented"
    CompileToBinary -> error "Not Yet Implemented"

readText :: Input -> IO Text
readText (FileInput path) = Txt.pack <$> readFile path
readText StdInput = Txt.pack <$> getContents
