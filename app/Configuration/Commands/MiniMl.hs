module Configuration.Commands.MiniMl (miniml) where

import Configuration.AppConfiguration (MiniMl (MiniMl), Debug (..))
import Configuration.Commands.Compile (compile)
import Configuration.Commands.Run (run)
import Configuration.Commands.Verify (verify)
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
    (appP <**> simpleVersioner "v0.1.0.0" <**> helper)
    ( fullDesc
        <> header "-- MiniML Runner & Compiler --"
        <> progDesc "MiniML is a minimal dialect of ML (Meta Language)"
    )

appP :: Parser MiniMl
appP = MiniMl <$> hsubparser (run <> compile <> verify) <*> debugP

debugP :: Parser Debug
debugP = flag defaultValue activeValue modifier
  where
    defaultValue = No
    activeValue = Yes
    modifier =
      long "debug"
        <> short 'd'
        <> help "Execute in debug mode"
