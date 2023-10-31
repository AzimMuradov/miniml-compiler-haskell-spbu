module Main where

import Data.Text (pack)
import Options.Applicative
import Parser.Parser (parseProgram)
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

-- * Main

main :: IO ()
main = runApp =<< execParser opts
  where
    opts =
      info
        (appP <**> helper)
        ( fullDesc
            <> header "-- MiniML Compiler --"
            <> progDesc "MiniML is a minimal dialect of ML (Meta Language)."
        )

-- ** Run app

runApp :: App -> IO ()
runApp App {input = i} = runApp' runAndShow i
  where
    runApp' :: (String -> String) -> Input -> IO ()
    runApp' f (FileInput path) = readFile path >>= \s -> putStr (f s)
    runApp' f StdInput = interact f

runAndShow :: String -> String
runAndShow fileText = runAndShow' <> "\n"
  where
    runAndShow' = case parseProgram (pack fileText) of
      Nothing -> "Please, try again. Can't parse your program."
      Just p -> case inferPolytype p of
        Left err -> pretty err
        Right pt -> pretty pt

-- ** App configuration

newtype App = App {input :: Input}
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
