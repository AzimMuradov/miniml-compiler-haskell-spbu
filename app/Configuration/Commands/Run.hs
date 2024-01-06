module Configuration.Commands.Run (run) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputParser)
import Data.Foldable (find)
import Data.List (intercalate)
import Options.Applicative

run :: Mod CommandFields Command
run = command "run" runParserInfo

runParserInfo :: ParserInfo Command
runParserInfo = info runParser runInfoMod

runParser :: Parser Command
runParser = CmdRun <$> (Run <$> inputParser <*> backendParser)

runInfoMod :: InfoMod a
runInfoMod =
  fullDesc
    <> header "Run MiniML program"
    <> progDesc "Run MiniML program with the provided backend"

backendParser :: Parser RunnerBackend
backendParser =
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
