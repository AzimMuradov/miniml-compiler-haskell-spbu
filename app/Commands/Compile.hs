{-# LANGUAGE OverloadedStrings #-}

module Commands.Compile (compile) where

import CodeGen.Llvm.Runner (compileToBinary, compileToLlvmIr)
import CodeGen.TimedValue (TimedValue (TimedValue))
import Configuration.AppConfiguration (CompilationTarget (..), Compile (..), Debug (Yes), Output (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Txt
import System.Exit (die)
import qualified Text.Printf as Printf
import Utils (inputToModuleName, ns2s, readText)

compile :: Compile -> Debug -> IO ()
compile (Compile input target output) debug = do
  let moduleName = inputToModuleName input
  text <- readText input

  TimedValue res compTime <- case target of
    TargetBinary -> do
      let outputFilePath = outputToFilePath output moduleName "out"
      compileToBinary moduleName text outputFilePath
    TargetLlvmIr -> do
      let outputFilePath = outputToFilePath output moduleName "ll"
      compileToLlvmIr moduleName text outputFilePath

  when (debug == Yes) $ do
    putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
    putStrLn ""

  either (die . Txt.unpack) return res

outputToFilePath :: Output -> Text -> Text -> FilePath
outputToFilePath output moduleName ext = case output of
  FileOutput filePath -> filePath
  AutoFileOutput -> Txt.unpack $ moduleName <> "." <> ext
