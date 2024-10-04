{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.PrintCRuntime (printCRuntime) where

import qualified CodeGen.Runtime.PrintRuntime as R
import Configuration.AppConfiguration (Output (..), PrintCRuntime (..))

printCRuntime :: PrintCRuntime -> IO ()
printCRuntime (PrintCRuntime output) = do
  R.printCRuntime $ outputToFilePath output

outputToFilePath :: Output -> FilePath
outputToFilePath = \case
  FileOutput filePath -> filePath
  AutoFileOutput -> "runtime.c"
