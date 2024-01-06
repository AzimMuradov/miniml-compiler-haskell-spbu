module Commands.Verify (verify) where

import CodeGen.Module (parseAndVerify)
import Configuration.AppConfiguration (Debug, Verify (..))
import qualified Data.Text as Txt
import System.Exit (die)
import Utils (readText)

verify :: Verify -> Debug -> IO ()
verify (Verify input) _ = do
  text <- readText input

  either (die . Txt.unpack) (const $ return ()) (parseAndVerify text)
