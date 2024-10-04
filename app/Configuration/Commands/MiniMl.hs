module Configuration.Commands.MiniMl (miniMl) where

import Configuration.AppConfiguration (Debug (..), MiniMl (MiniMl))
import Configuration.Commands.Compile (compile)
import Configuration.Commands.PrintCRuntime (printCRuntime)
import Configuration.Commands.Run (run)
import Options.Applicative

miniMl :: IO MiniMl
miniMl = customExecParser miniMlParserPrefs miniMlParserInfo

miniMlParserPrefs :: ParserPrefs
miniMlParserPrefs =
  prefs $
    showHelpOnError
      <> showHelpOnEmpty
      <> subparserInline
      <> helpShowGlobals

miniMlParserInfo :: ParserInfo MiniMl
miniMlParserInfo = info (miniMlParser <**> helper) miniMlInfoMod

miniMlParser :: Parser MiniMl
miniMlParser = MiniMl <$> hsubparser (run <> compile <> printCRuntime) <*> debugParser

miniMlInfoMod :: InfoMod a
miniMlInfoMod =
  fullDesc
    <> header "-- MiniML Runner & Compiler --"
    <> progDesc "MiniML is a minimal dialect of ML (Meta Language)"

debugParser :: Parser Debug
debugParser = flag defaultValue activeValue modifier
  where
    defaultValue = No
    activeValue = Yes
    modifier =
      long "debug"
        <> short 'd'
        <> help "Execute in debug mode"
