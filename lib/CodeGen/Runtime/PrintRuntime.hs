{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Runtime.PrintRuntime (printCRuntime) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.Text.Encoding as Txt
import qualified Data.Text.IO as Txt
import System.IO (IOMode (WriteMode), withFile)

printCRuntime :: FilePath -> IO ()
printCRuntime outputFilePath = do
  let runtimeFileText = Txt.decodeUtf8 $(makeRelativeToProject "lib/CodeGen/Runtime/runtime.c" >>= embedFile)
  withFile outputFilePath WriteMode $ \handle -> do
    Txt.hPutStr handle runtimeFileText
