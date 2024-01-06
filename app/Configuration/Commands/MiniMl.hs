module Configuration.Commands.MiniMl (miniml) where

import Configuration.AppConfiguration (Debug (..), MiniMl (MiniMl))
import Configuration.Commands.Compile (compile)
import Configuration.Commands.Run (run)
import Options.Applicative

miniml :: IO MiniMl
miniml = customExecParser parserPrefs parserInfo

parserPrefs :: ParserPrefs
parserPrefs =
  prefs $
    showHelpOnError
      <> showHelpOnEmpty
      <> subparserInline
      <> helpShowGlobals

parserInfo :: ParserInfo MiniMl
parserInfo =
  info
    (appP <**> helper)
    ( fullDesc
        <> header "-- MiniML Runner & Compiler --"
        <> progDesc "MiniML is a minimal dialect of ML (Meta Language)"
    )

appP :: Parser MiniMl
appP = MiniMl <$> hsubparser (run <> compile) <*> debugP

debugP :: Parser Debug
debugP = flag defaultValue activeValue modifier
  where
    defaultValue = No
    activeValue = Yes
    modifier =
      long "debug"
        <> short 'd'
        <> help "Execute in debug mode"
