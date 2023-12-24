module CodeGen.Module where

import Data.Text (Text)
import qualified Transformations.Anf.Anf as Anf

data Module = Module
  { name :: Text,
    code :: Anf.Program
  }
