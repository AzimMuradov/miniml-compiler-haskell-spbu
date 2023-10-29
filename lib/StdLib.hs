{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import Parser.Ast

stdDeclarations :: [StdDeclaration]
stdDeclarations = [notDecl, printBool, printInt]

notDecl :: StdDeclaration
notDecl = StdDecl "not" (TFun TBool TBool)

printBool :: StdDeclaration
printBool = StdDecl "print_bool" (TFun TBool TUnit)

printInt :: StdDeclaration
printInt = StdDecl "print_int" (TFun TInt TUnit)
