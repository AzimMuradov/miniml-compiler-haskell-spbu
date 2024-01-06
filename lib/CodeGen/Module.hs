{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Module
  ( Module (Module, name, code),
    compileToModule,
    parseAndVerify,
  )
where

import Control.Monad.Except (Except, runExcept)
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
import qualified TypeChecker.TypeChecker as TC

data Module = Module
  { name :: Text,
    code :: Anf.Program
  }
  deriving (Show, Eq)

compileToModule :: Text -> Text -> Except Text Module
compileToModule moduleName text = do
  program <- parseAndVerify' text
  let astToAnf = genAnf . llAst . ccAst . relabelAst . simplifyAst
      anf = astToAnf program
      irMod = Module moduleName anf
   in return irMod

parseAndVerify :: Text -> Either Text Program
parseAndVerify = runExcept . parseAndVerify'

parseAndVerify' :: Text -> Except Text Program
parseAndVerify' text = case parseProgram text of
  Just program -> case TC.checkProgram program of
    Right () -> return program
    Left e -> throwE $ "Error: " <> Txt.pack (TC.pretty e)
  Nothing -> throwE "Error: Syntax error"
