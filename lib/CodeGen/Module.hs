module CodeGen.Module where

import Data.Text (Text)
import Parser.Parser (parseProgram)
import qualified Transformations.Anf.Anf as Anf
import Transformations.Anf.AnfGen (genAnf)
import Transformations.Cc.Cc (ccAst)
import Transformations.Ll.Ll (llAst)
import Transformations.Relabeler.Relabeler (relabelAst)
import Transformations.Simplifier.Simplifier (simplifyAst)
import TypeChecker.HindleyMilner (TypeError)
import TypeChecker.TypeChecker (checkProgram)

data Module = Module
  { name :: Text,
    code :: Anf.Program
  }
  deriving (Show, Eq)

data CompilationResult
  = Success Module
  | SyntaxError
  | SemanticError TypeError

compileToModule :: Text -> Text -> CompilationResult
compileToModule moduleName text = case parseProgram text of
  Just ast -> case checkProgram ast of
    Right () ->
      let astToAnf = genAnf . llAst . ccAst . relabelAst . simplifyAst
          anf = astToAnf ast
          irMod = Module moduleName anf
       in Success irMod
    Left e -> SemanticError e
  Nothing -> SyntaxError
