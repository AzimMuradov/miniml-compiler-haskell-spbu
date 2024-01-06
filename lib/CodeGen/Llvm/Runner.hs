{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Llvm.Runner (run, compileToBinary, compileToLlvmIr) where

import CodeGen.Llvm.LlvmIrGen (genLlvmIrModule, ppLlvmModule)
import CodeGen.Module (compileToModule)
import qualified CodeGen.RunResult as RR
import CodeGen.TimedValue (TimedValue (TimedValue), measureTimedValue)
import Control.Exception (bracket)
import Control.Monad.Except (Except, runExcept)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import qualified Data.Text.IO as Txt
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (IOMode (WriteMode), hClose, withFile)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess, readProcessWithExitCode)

run :: Text -> Text -> IO RR.RunResult
run moduleName text = do
  TimedValue compResult compTime <- compileToBinary moduleName text outputFilePath

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

    runCompiledModule :: IO (TimedValue (Either (String, String, Int) String))
    runCompiledModule = do
      measuredResult <- measureTimedValue $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode outputFilePath [] []
        return $ case exitCode of
          ExitSuccess -> Right stdout
          ExitFailure ec -> Left (stdout, stderr, ec)

      removePathForcibly outputFilePath

      return measuredResult

compileToBinary :: Text -> Text -> FilePath -> IO (TimedValue (Either Text ()))
compileToBinary moduleName text outputFilePath = measureTimedValue $
  sequenceA $
    runExcept $ do
      llvmIrText <- compileToLlvmIr' moduleName text
      return $
        bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
          withCurrentDirectory buildDir $ do
            (llvm, llvmHandle) <- mkstemps (Txt.unpack moduleName) ".ll"
            Txt.hPutStrLn llvmHandle llvmIrText
            hClose llvmHandle

            (runtime, runtimeHandle) <- mkstemps "runtime" ".c"
            let runtimeFileText = Txt.decodeUtf8 $(makeRelativeToProject "lib/CodeGen/Runtime/runtime.c" >>= embedFile)
            Txt.hPutStrLn runtimeHandle runtimeFileText
            hClose runtimeHandle

            callProcess "clang" ["-Wno-override-module", "-O3", "-lm", llvm, runtime, "-o", "../" <> outputFilePath]

compileToLlvmIr :: Text -> Text -> FilePath -> IO (TimedValue (Either Text ()))
compileToLlvmIr moduleName text outputFilePath = measureTimedValue $
  sequenceA $
    runExcept $ do
      llvmIrText <- compileToLlvmIr' moduleName text
      return $
        withFile outputFilePath WriteMode $ \handle -> do
          Txt.hPutStrLn handle llvmIrText

-- * Internal

compileToLlvmIr' :: Text -> Text -> Except Text Text
compileToLlvmIr' moduleName text = do
  irModule <- compileToModule moduleName text
  return $ ppLlvmModule $ genLlvmIrModule irModule
