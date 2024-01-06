{-# LANGUAGE OverloadedStrings #-}

module Commands.Run (run) where

import qualified CodeGen.Llvm.Runner as Llvm
import CodeGen.RunResult (RunResult (CompilationError, RuntimeError, Success))
import Configuration.AppConfiguration (Debug (Yes), Input (..), Run (Run), RunnerBackend (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Txt
import System.Exit (ExitCode (..), die, exitWith)
import System.FilePath (takeBaseName)
import System.IO (hPutStr)
import qualified System.IO as Sys
import qualified Text.Printf as Printf
import Utils (ns2s, readText)

run :: Run -> Debug -> IO ()
run (Run input backend) debug = do
  text <- readText input

  case backend of
    BackendLlvm -> do
      let moduleName = case input of
            StdInput -> "unnamed"
            FileInput s -> Txt.pack $ takeBaseName s
      runLlvm moduleName text debug
    BackendInterpreter -> error "Not Yet Implemented"

runLlvm :: Text -> Text -> Debug -> IO ()
runLlvm moduleName text debug = do
  runResult <- Llvm.run moduleName text

  case runResult of
    Success stdout compTime runTime -> do
      when (debug == Yes) $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn $ Printf.printf "Finished running in %0.5f sec" (ns2s runTime)
        putStrLn ""

      putStr $ Txt.unpack stdout
    CompilationError compErrMsg compTime -> do
      when (debug == Yes) $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn ""

      die $ Txt.unpack compErrMsg
    RuntimeError stdout stderr exitCode compTime runTime -> do
      when (debug == Yes) $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn $ Printf.printf "Finished running in %0.5f sec" (ns2s runTime)
        putStrLn ""

      putStr $ Txt.unpack stdout

      hPutStr Sys.stderr (Txt.unpack stderr)

      exitWith $ ExitFailure exitCode
