module Configuration.Commands.Verify (verify) where

import Configuration.AppConfiguration
import Configuration.CommonParsers (inputP)
import Options.Applicative

verify :: Mod CommandFields Command
verify =
  command "verify" $
    info
      (CmdVerify <$> verifyP)
      ( fullDesc
          <> header "Verify MiniML program"
          <> progDesc "Verify MiniML program for syntactic and semantic correctness"
      )

verifyP :: Parser Verify
verifyP = Verify <$> inputP
