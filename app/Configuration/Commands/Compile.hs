module Configuration.Commands.Compile (compile) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputParser)
import Data.Foldable (find)
import Data.List (intercalate)
import Options.Applicative

compile :: Mod CommandFields Command
compile = command "compile" compileParserInfo

compileParserInfo :: ParserInfo Command
compileParserInfo = info compileParser compileInfoMod

compileParser :: Parser Command
compileParser =
  CmdCompile
    <$> (Compile <$> inputParser <*> targetParser <*> outputParser)

compileInfoMod :: InfoMod a
compileInfoMod =
  fullDesc
    <> header "Compile MiniML program"
    <> progDesc "Compile MiniML program to the provided target"

targetParser :: Parser CompilationTarget
targetParser =
  option
    (maybeReader reader)
    ( long "target"
        <> short 't'
        <> metavar "TARGET"
        <> help helpMsg
        <> showDefault
        <> value TargetBinary
    )
  where
    reader s = find (\e -> show e == s) targets

    helpMsg = "Compilation target (" <> intercalate "|" (show <$> targets) <> ")"

    targets :: [CompilationTarget]
    targets = [minBound .. maxBound]

outputParser :: Parser Output
outputParser = fileOutputP <|> defaultP
  where
    fileOutputP =
      FileOutput
        <$> strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output filename (default: taken from input filename)"
          )

    defaultP = pure AutoFileOutput
