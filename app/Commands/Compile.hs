{-# LANGUAGE OverloadedStrings #-}

module Commands.Compile (compile) where

import CodeGen.Llvm.Runner (compileToBin, compileToIr)
import CodeGen.TimedValue (TimedValue (TimedValue))
import Configuration.AppConfiguration (CompilationTarget (..), Compile (..), Debug (Yes), Input (..), Output (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Txt
import System.Exit (die)
import System.FilePath (takeBaseName)
import qualified Text.Printf as Printf
import Utils (ns2s, readText)

compile :: Compile -> Debug -> IO ()
compile (Compile input target output) debug = do
  text <- readText input

  let moduleName = case input of
        StdInput -> "unnamed"
        FileInput filePath -> Txt.pack $ takeBaseName filePath

  TimedValue res compTime <- case target of
    TargetBinary -> do
      let out = outputToFilePath output moduleName "out"
      compileToBin moduleName text out
    TargetLlvmIr -> do
      let out = outputToFilePath output moduleName "ll"
      compileToIr moduleName text out

  when (debug == Yes) $ do
    putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
    putStrLn ""

  either (die . Txt.unpack) return res

outputToFilePath :: Output -> Text -> Text -> FilePath
outputToFilePath output moduleName ext = case output of
  FileOutput filePath -> filePath
  AutoFileOutput -> Txt.unpack $ moduleName <> "." <> ext
