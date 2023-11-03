module Transformations.AstToAnf (astToAnf) where

import qualified Parser.Ast as Ast
import qualified Transformations.Anf as Anf
import Transformations.AstToTypelessAst (astToTypelessAst)
import Transformations.TypelessAstToAnf (typelessAstToAnf)

astToAnf :: Ast.Program -> Anf.Program
astToAnf = typelessAstToAnf . astToTypelessAst
