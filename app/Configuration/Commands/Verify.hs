module Configuration.Commands.Verify (verify) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputParser)
import Options.Applicative

verify :: Mod CommandFields Command
verify = command "verify" verifyParserInfo

verifyParserInfo :: ParserInfo Command
verifyParserInfo = info verifyParser verifyInfoMod

verifyParser :: Parser Command
verifyParser = CmdVerify . Verify <$> inputParser

verifyInfoMod :: InfoMod a
verifyInfoMod =
  fullDesc
    <> header "Verify MiniML program"
    <> progDesc "Verify MiniML program for syntactic and semantic correctness"
