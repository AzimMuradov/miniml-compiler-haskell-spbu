{-# LANGUAGE OverloadedStrings #-}

module StdLib
  ( TypedDeclaration,
    allTypedDecls,
    typedDecls,
    decls,
  )
where

import Trees.Common (Identifier, Type (..))

-- * Standard Library

type TypedDeclaration = (Identifier, Type)

-- | The list of all (including internal) typed standard declarations.
allTypedDecls :: [TypedDeclaration]
allTypedDecls = typedDecls <> [divDecl, funToPafDecl, applyDecl]

-- | The list of typed standard declarations.
typedDecls :: [TypedDeclaration]
typedDecls = [notDecl, printBoolDecl, printIntDecl]

-- | The list of standard declarations.
decls :: [Identifier]
decls = fst <$> typedDecls

-- ** Function Declarations

-- | The @not@ function declaration (@not : bool -> bool@).
notDecl :: TypedDeclaration
notDecl = ("not", TFun TBool TBool)

-- | The @print_bool@ function declaration (@print_bool : bool -> unit@).
printBoolDecl :: TypedDeclaration
printBoolDecl = ("print_bool", TFun TBool TUnit)

-- | The @print_int@ function declaration (@print_int : int -> unit@).
printIntDecl :: TypedDeclaration
printIntDecl = ("print_int", TFun TInt TUnit)

-- ** Internal Function Declarations

divDecl :: TypedDeclaration
divDecl = ("miniml_div", TFun TInt (TFun TInt TInt))

funToPafDecl :: TypedDeclaration
funToPafDecl = ("miniml_fun_to_paf", TFun TInt (TFun TInt TInt))

applyDecl :: TypedDeclaration
applyDecl = ("miniml_apply", TFun TInt (TFun TInt TInt))
