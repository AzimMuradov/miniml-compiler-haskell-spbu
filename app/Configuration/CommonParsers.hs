module Configuration.CommonParsers where

import Configuration.AppConfiguration (Input (..))
import Options.Applicative

inputParser :: Parser Input
inputParser = fileInputP <|> defaultP
  where
    fileInputP =
      FileInput
        <$> strArgument
          ( metavar "FILE"
              <> help "Program file path (optional)"
          )

    defaultP = pure StdInput
