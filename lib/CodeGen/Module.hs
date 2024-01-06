{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Module where

import Data.Text (Text)
import qualified Data.Text as Txt
import Parser.Parser (parseProgram)
import qualified Transformations.Anf.Anf as Anf
import Transformations.Anf.AnfGen (genAnf)
import Transformations.Cc.Cc (ccAst)
import Transformations.Ll.Ll (llAst)
import Transformations.Relabeler.Relabeler (relabelAst)
import Transformations.Simplifier.Simplifier (simplifyAst)
import qualified TypeChecker.PrettyPrinter as TC
import TypeChecker.TypeChecker (checkProgram)

data Module = Module
  { name :: Text,
    code :: Anf.Program
  }
  deriving (Show, Eq)

compileToModule :: Text -> Text -> Either Text Module
compileToModule moduleName text = case parseProgram text of
  Just ast -> case checkProgram ast of
    Right () ->
      let astToAnf = genAnf . llAst . ccAst . relabelAst . simplifyAst
          anf = astToAnf ast
          irMod = Module moduleName anf
       in Right irMod
    Left e -> Left $ "Error: " <> Txt.pack (TC.pretty e)
  Nothing -> Left "Error: Syntax error"
