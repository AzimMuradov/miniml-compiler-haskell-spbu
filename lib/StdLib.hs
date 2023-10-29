{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import Parser.Ast

stdDeclarations :: [Statement]
stdDeclarations = [notDecl]

notDecl :: Statement
notDecl = StmtStdDecl "not" (TFun TBool TBool)
