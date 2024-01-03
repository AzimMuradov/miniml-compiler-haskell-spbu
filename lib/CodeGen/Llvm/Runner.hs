{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Llvm.Runner (run) where

import CodeGen.Llvm.Ir2LlvmIr (genLlvmIrModule)
import CodeGen.Module (compileToModule)
import qualified CodeGen.Module as M
import qualified CodeGen.RunResult as RR
import CodeGen.TimedValue (TimedValue (TimedValue), measureTimedValue)
import Control.Exception (bracket)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import qualified Data.Text.IO as Txt
import qualified LLVM.AST as LLVM
import LLVM.Pretty (ppllvm)
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess, readProcessWithExitCode)
import qualified TypeChecker.PrettyPrinter as TC

run :: Text -> Text -> IO RR.RunResult
run moduleName text = do
  TimedValue compResult compTime <- runCompilation

  case compResult of
    Right () -> do
      TimedValue runResult runTime <- runCompiledModule
      return $ case runResult of
        Right out ->
          RR.Success
            { RR.stdout = cs out,
              RR.compTime = compTime,
              RR.runTime = runTime
            }
        Left (out, err, code) ->
          RR.RuntimeError
            { RR.stdout = cs out,
              RR.stderr = cs err,
              RR.exitCode = code,
              RR.compTime = compTime,
              RR.runTime = runTime
            }
    Left compErrMsg ->
      return $
        RR.CompilationError
          { RR.compErrMsg = compErrMsg,
            RR.compTime = compTime
          }
  where
    outputFilePath :: FilePath
    outputFilePath = "./" <> Txt.unpack moduleName

    runCompilation :: IO (TimedValue (Either Text ()))
    runCompilation = do
      measureTimedValue $ do
        case compileToModule moduleName text of
          M.Success irModule -> do
            let llvmModule = genLlvmIrModule irModule
            Right () <$ compile llvmModule moduleName outputFilePath
          M.SyntaxError -> return $ Left "Error: Syntax error"
          M.SemanticError te -> return $ Left $ Txt.pack $ TC.pretty te

    runCompiledModule :: IO (TimedValue (Either (String, String, Int) String))
    runCompiledModule = do
      measuredResult <- measureTimedValue $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode outputFilePath [] []
        return $ case exitCode of
          ExitSuccess -> Right stdout
          ExitFailure ec -> Left (stdout, stderr, ec)

      removePathForcibly outputFilePath

      return measuredResult

compile :: LLVM.Module -> Text -> FilePath -> IO ()
compile llvmModule moduleName outputFilePath =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      (llvm, llvmHandle) <- mkstemps (Txt.unpack moduleName) ".ll"
      let llvmModuleText = cs (ppllvm llvmModule)
      -- Txt.putStrLn llvmModuleText -- For debug
      Txt.hPutStrLn llvmHandle llvmModuleText
      hClose llvmHandle

      (runtime, runtimeHandle) <- mkstemps "runtime" ".c"
      let runtimeFileText = Txt.decodeUtf8 $(makeRelativeToProject "lib/CodeGen/Runtime/runtime.c" >>= embedFile)
      Txt.hPutStrLn runtimeHandle runtimeFileText
      hClose runtimeHandle

      callProcess "clang" ["-Wno-override-module", "-O0", "-lm", llvm, runtime, "-o", "../" <> outputFilePath]
