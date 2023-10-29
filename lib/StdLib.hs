{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import Parser.Ast

stdDeclarations :: [Statement]
stdDeclarations = [notDecl, printBool, printInt]

notDecl :: Statement
notDecl = StmtStdDecl "not" (TFun TBool TBool)

printBool :: Statement
printBool = StmtStdDecl "print_bool" (TFun TBool TUnit)

printInt :: Statement
printInt = StmtStdDecl "print_int" (TFun TInt TUnit)
