module Configuration.Commands.Compile (compile) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputP)
import Data.Foldable (find)
import Data.List (intercalate)
import Options.Applicative

compile :: Mod CommandFields Command
compile =
  command "compile" $
    info
      (CmdCompile <$> compileP)
      ( fullDesc
          <> header "Compile MiniML program"
          <> progDesc "Compile MiniML program to the provided target"
      )

compileP :: Parser Compile
compileP = Compile <$> inputP <*> targetP <*> outputP

targetP :: Parser CompilationTarget
targetP =
  option
    (maybeReader reader)
    ( long "target"
        <> short 't'
        <> metavar "TARGET"
        <> help helpMsg
        <> showDefault
        <> value CompileToBinary
    )
  where
    reader s = find (\e -> show e == s) targets

    helpMsg = "Compilation target (" <> intercalate "|" (show <$> targets) <> ")"

    targets :: [CompilationTarget]
    targets = [minBound .. maxBound]

outputP :: Parser Output
outputP = fileOutputP <|> defaultP
  where
    fileOutputP =
      FileOutput
        <$> strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file"
          )

    defaultP = pure AutoFileOutput
