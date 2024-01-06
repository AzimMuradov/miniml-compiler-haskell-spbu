{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import CodeGen.TimedValue (Nanoseconds (..))
import Configuration.AppConfiguration (Input (..))
import Data.Text (Text)
import qualified Data.Text as Txt
import System.FilePath (takeBaseName)

readText :: Input -> IO Text
readText (FileInput path) = Txt.pack <$> readFile path
readText StdInput = Txt.pack <$> getContents

inputToModuleName :: Input -> Text
inputToModuleName = \case
  StdInput -> "unnamed"
  FileInput filePath -> Txt.pack $ takeBaseName filePath

ns2s :: Nanoseconds -> Double
ns2s ns = let Nanoseconds ns' = ns in fromInteger ns' / 1_000_000_000
