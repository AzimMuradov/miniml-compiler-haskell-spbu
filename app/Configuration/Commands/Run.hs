module Configuration.Commands.Run (run) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputParser)
import Options.Applicative

run :: Mod CommandFields Command
run = command "run" runParserInfo

runParserInfo :: ParserInfo Command
runParserInfo = info runParser runInfoMod

runParser :: Parser Command
runParser = CmdRun <$> (Run <$> inputParser)

runInfoMod :: InfoMod a
runInfoMod =
  fullDesc
    <> header "Run MiniML program"
    <> progDesc "Run MiniML program with the provided backend"
