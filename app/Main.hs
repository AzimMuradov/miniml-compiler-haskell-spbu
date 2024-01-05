{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CodeGen.Llvm.Runner as Llvm
import CodeGen.RunResult (RunResult (CompilationError, RuntimeError, Success))
import CodeGen.TimedValue (Nanoseconds (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Txt
import Options.Applicative hiding (Success)
import System.Exit (ExitCode (..), die, exitWith)
import System.FilePath (takeBaseName)
import System.IO (hPutStr)
import qualified System.IO as Sys
import qualified Text.Printf as Printf

-- * Main

main :: IO ()
main = run True =<< execParser opts
  where
    opts =
      info
        (appP <**> helper)
        ( fullDesc
            <> header "-- MiniML Compiler --"
            <> progDesc "MiniML is a minimal dialect of ML (Meta Language)."
        )

-- ** Run app

run :: Bool -> App -> IO ()
run debug (App input) = do
  text <- readText input

  let moduleName = case input of
        StdInput -> "unnamed"
        FileInput s -> Txt.pack $ takeBaseName s
  runLlvm debug moduleName text

runLlvm :: Bool -> Text -> Text -> IO ()
runLlvm debug moduleName text = do
  runResult <- Llvm.run moduleName text

  case runResult of
    Success stdout compTime runTime -> do
      when debug $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn $ Printf.printf "Finished running in %0.5f sec" (ns2s runTime)
        putStrLn ""

      putStr $ Txt.unpack stdout
    CompilationError compErrMsg compTime -> do
      when debug $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn ""

      die $ Txt.unpack compErrMsg
    RuntimeError stdout stderr exitCode compTime runTime -> do
      when debug $ do
        putStrLn $ Printf.printf "Finished compiling in %0.5f sec" (ns2s compTime)
        putStrLn $ Printf.printf "Finished running in %0.5f sec" (ns2s runTime)
        putStrLn ""

      putStr $ Txt.unpack stdout

      hPutStr Sys.stderr (Txt.unpack stderr)

      exitWith $ ExitFailure exitCode

readText :: Input -> IO Text
readText (FileInput path) = Txt.pack <$> readFile path
readText StdInput = Txt.pack <$> getContents

ns2s :: Nanoseconds -> Double
ns2s ns = let Nanoseconds ns' = ns in fromInteger ns' / 1_000_000_000

-- ** App configuration

newtype App = App Input
  deriving (Show)

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)

appP :: Parser App
appP = App <$> inputP

-- ** Command line options parsing

inputP :: Parser Input
inputP = fileInputP <|> stdInputP
  where
    stdInputP = pure StdInput
    fileInputP =
      FileInput
        <$> strOption
          ( long "file"
              <> short 'f'
              <> metavar "FILENAME"
              <> help "Read from the file (optional)"
          )
