module Configuration.CommonParsers where

import Configuration.AppConfiguration (Input (..))
import Options.Applicative

inputP :: Parser Input
inputP = fileInputP <|> defaultP
  where
    fileInputP =
      FileInput
        <$> strArgument
          ( metavar "FILENAME"
              <> help "Read from the file (optional)"
          )

    defaultP = pure StdInput
