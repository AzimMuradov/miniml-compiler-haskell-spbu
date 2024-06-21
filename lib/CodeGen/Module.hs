{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Module
  ( Module (Module, code),
    compileToModule,
  )
where

import Control.Monad.Except (Except)
import Control.Monad.Trans.Except (throwE)
import Data.Text (Text)
import qualified Data.Text as Txt
import Parser.Ast (Program)
import Parser.Parser (parseProgram)
import qualified Transformations.Anf.Anf as Anf
import Transformations.Anf.AnfGen (genAnf)
import Transformations.Cc.Cc (ccAst)
import Transformations.Ll.Ll (llAst)
import Transformations.Relabeler.Relabeler (relabelAst)
import Transformations.Simplifier.Simplifier (simplifyAst)
import qualified TypeChecker.PrettyPrinter as TC
import TypeChecker.TypeChecker (checkProgram)

newtype Module = Module {code :: Anf.Program}
  deriving (Show, Eq)

compileToModule :: Text -> Except Text Module
compileToModule text = do
  program <- parseAndVerify text
  let astToAnf = genAnf . llAst . ccAst . relabelAst . simplifyAst
      anf = astToAnf program
      irMod = Module anf
   in return irMod

parseAndVerify :: Text -> Except Text Program
parseAndVerify text = case parseProgram text of
  Just program -> case checkProgram program of
    Right () -> return program
    Left e -> throwE $ "Error: " <> Txt.pack (TC.pretty e)
  Nothing -> throwE "Error: Syntax error"
