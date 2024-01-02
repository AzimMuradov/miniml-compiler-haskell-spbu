module Utils
  ( processTillParser,
    processTillTypeChecker,
    processTillAstSimplifier,
    processTillRelabeler,
    processTillCc,
    processTillLl,
    processTillAnfGen,
    processTillLlvmIr,
  )
where

import CodeGen.Llvm.Ir2LlvmIr (genLlvmIrModule, ppLlvmModule)
import CodeGen.Module (Module (Module))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as Txt
import qualified Parser.Ast as Ast
import Parser.Parser (parseProgram)
import qualified Transformations.Anf.Anf as Anf
import Transformations.Anf.AnfGen (genAnf)
import qualified Transformations.Anf.PrettyPrinter as Anf
import Transformations.Cc.Cc (ccAst)
import qualified Transformations.Ll.Lfr as Lfr
import Transformations.Ll.Ll (llAst)
import Transformations.Relabeler.Relabeler (relabelAst)
import qualified Transformations.Simplifier.SimplifiedAst as SAst
import Transformations.Simplifier.Simplifier (simplifyAst)
import TypeChecker.HindleyMilner (Polytype, TypeError)
import qualified TypeChecker.PrettyPrinter as TC
import TypeChecker.TypeChecker (inferProgram)

processTillParser :: Text -> Maybe Ast.Program
processTillParser = processTillParser'

processTillTypeChecker :: Text -> String
processTillTypeChecker = either TC.pretty TC.pretty . processTillTypeChecker'

processTillAstSimplifier :: Text -> String
processTillAstSimplifier = show . processTillAstSimplifier'

processTillRelabeler :: Text -> String
processTillRelabeler = show . processTillRelabeler'

processTillCc :: Text -> String
processTillCc = show . processTillCc'

processTillLl :: Text -> String
processTillLl = show . processTillLl'

processTillAnfGen :: Text -> String
processTillAnfGen = Anf.prettyPrint . processTillAnfGen'

processTillLlvmIr :: Text -> Text -> String
processTillLlvmIr name program = Txt.unpack $ ppLlvmModule $ genLlvmIrModule (Module name (processTillAnfGen' program))

-- Combinators

processTillParser' :: Text -> Maybe Ast.Program
processTillParser' = parseProgram

processTillTypeChecker' :: Text -> Either TypeError Polytype
processTillTypeChecker' = inferProgram . fromJust . processTillParser'

processTillAstSimplifier' :: Text -> SAst.Program
processTillAstSimplifier' = simplifyAst . fromJust . processTillParser'

processTillRelabeler' :: Text -> SAst.Program
processTillRelabeler' = relabelAst . processTillAstSimplifier'

processTillCc' :: Text -> SAst.Program
processTillCc' = ccAst . processTillRelabeler'

processTillLl' :: Text -> Lfr.Program
processTillLl' = llAst . processTillCc'

processTillAnfGen' :: Text -> Anf.Program
processTillAnfGen' = genAnf . processTillLl'