{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import Parser.Ast

stdDeclarations :: [Statement]
stdDeclarations = [notDecl, printInt, printBool]

notDecl :: Statement
notDecl = StmtStdDecl "not" (TFun TBool TBool)

printInt :: Statement
printInt = StmtStdDecl "print_int" (TFun TInt TUnit)

printBool :: Statement
printBool = StmtStdDecl "print_bool" (TFun TBool TUnit)
