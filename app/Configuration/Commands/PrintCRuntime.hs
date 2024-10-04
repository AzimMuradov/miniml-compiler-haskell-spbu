module Configuration.Commands.PrintCRuntime (printCRuntime) where

import Configuration.AppConfiguration
import Options.Applicative

printCRuntime :: Mod CommandFields Command
printCRuntime = command "print-c-runtime" compileParserInfo

compileParserInfo :: ParserInfo Command
compileParserInfo = info printCRuntimeParser printCRuntimeInfoMod

printCRuntimeParser :: Parser Command
printCRuntimeParser =
  CmdPrintCRuntime . PrintCRuntime <$> outputParser

printCRuntimeInfoMod :: InfoMod a
printCRuntimeInfoMod =
  fullDesc
    <> header "Print C runtime"
    <> progDesc "Print C runtime"

outputParser :: Parser Output
outputParser = fileOutputP <|> defaultP
  where
    fileOutputP =
      FileOutput
        <$> strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file path (default: runtime.c)"
          )

    defaultP = pure AutoFileOutput
