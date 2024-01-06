module Configuration.CommonParsers where

import Configuration.AppConfiguration (Input (..))
import Options.Applicative

inputParser :: Parser Input
inputParser = fileInputP <|> defaultP
  where
    fileInputP =
      FileInput
        <$> strArgument
          ( metavar "FILENAME"
              <> help "Read from the provided file (optional)"
          )

    defaultP = pure StdInput
