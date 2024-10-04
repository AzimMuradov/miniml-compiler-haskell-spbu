module CodeGen.RiscV.Runner (compileToRiscVAsm) where

import CodeGen.Module (compileToModule)
import CodeGen.RiscV.AsmGen (ppRiscVAsm)
import CodeGen.TimedValue (TimedValue, measureTimedValue)
import Control.Monad.Except (runExcept)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import System.IO (IOMode (WriteMode), withFile)

compileToRiscVAsm :: Text -> FilePath -> IO (TimedValue (Either Text ()))
compileToRiscVAsm text outputFilePath = measureTimedValue $
  sequenceA $
    runExcept $ do
      m <- compileToModule text
      let riscVText = Txt.pack $ ppRiscVAsm m
      return $
        withFile outputFilePath WriteMode $ \handle -> do
          Txt.hPutStr handle riscVText
