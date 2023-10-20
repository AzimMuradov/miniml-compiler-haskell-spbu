{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import qualified Data.List.NonEmpty as NonEmpty
import Parser.Ast

stdDeclarations :: [Statement]
stdDeclarations = [notDecl]

notDecl :: Statement
notDecl = StmtFunDecl $ FunDecl name (Fun params returnType fakeBody)
  where
    name = "not"
    params = NonEmpty.singleton ("x", Just TBool)
    returnType = Just TBool
    fakeBody = ExprIf (ExprIdentifier "x") (ExprValue $ ValBool False) (ExprValue $ ValBool True)
