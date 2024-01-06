module Components.Verify (verify) where

import Configuration.AppConfiguration (Debug (Yes), Input (..), Verify (Verify))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Txt
import Parser.Parser (parseProgram)
import System.Exit (die)
import Text.Pretty.Simple (pPrint)
import TypeChecker.PrettyPrinter (pretty)
import TypeChecker.TypeChecker (checkProgram)

verify :: Verify -> Debug -> IO ()
verify (Verify input) debug = do
  text <- readText input

  let program = parseProgram text
  p <- maybe (die "Error: Syntax error") return program
  when (debug == Yes) $ do
    pPrint p
    putStrLn ""

  either (die . pretty) return (checkProgram p)
  putStrLn "Ok"

readText :: Input -> IO Text
readText (FileInput path) = Txt.pack <$> readFile path
readText StdInput = Txt.pack <$> getContents
