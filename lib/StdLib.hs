{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdDeclarations) where

import Parser.Ast

-- | The list of standard declarations.
stdDeclarations :: [StdDeclaration]
stdDeclarations = [notDecl, printBoolDecl, printIntDecl]

-- | The @not@ function declaration (@not : bool -> bool@).
notDecl :: StdDeclaration
notDecl = StdDecl "not" (TFun TBool TBool)

-- | The @print_bool@ function declaration (@print_bool : bool -> unit@).
printBoolDecl :: StdDeclaration
printBoolDecl = StdDecl "print_bool" (TFun TBool TUnit)

-- | The @print_int@ function declaration (@print_int : int -> unit@).
printIntDecl :: StdDeclaration
printIntDecl = StdDecl "print_int" (TFun TInt TUnit)
