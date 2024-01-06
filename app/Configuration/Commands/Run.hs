module Configuration.Commands.Run (run) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputP)
import Data.Foldable (find)
import Data.List (intercalate)
import Options.Applicative

run :: Mod CommandFields Command
run =
  command "run" $
    info
      (CmdRun <$> runP)
      ( fullDesc
          <> header "Run MiniML program"
          <> progDesc "Run MiniML program with the provided backend"
      )

runP :: Parser Run
runP = Run <$> inputP <*> backendP

backendP :: Parser RunnerBackend
backendP =
  option
    (maybeReader reader)
    ( long "backend"
        <> short 'b'
        <> metavar "BACKEND"
        <> help helpMsg
        <> showDefault
        <> value BackendLlvm
    )
  where
    reader s = find (\e -> show e == s) backends

    helpMsg = "Runner backend (" <> intercalate "|" (show <$> backends) <> ")"

    backends :: [RunnerBackend]
    backends = [minBound .. maxBound]
