{-# LANGUAGE OverloadedStrings #-}

module StdLib
  ( TypedDeclaration,
    typedDecls,
    decls,
    DeclarationWithArity,
    allDeclsWithArity,
  )
where

import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (hylo)
import Trees.Common (Arity, Identifier, Type (..))

-- * Standard Library

type TypedDeclaration = (Identifier, Type)

-- | The list of typed standard declarations.
typedDecls :: [TypedDeclaration]
typedDecls = [notDecl, printBoolDecl, printIntDecl]

-- | The list of standard declarations.
decls :: [Identifier]
decls = fst <$> typedDecls

type DeclarationWithArity = (Identifier, Arity)

-- | The list of all (including internal) standard declarations with their arity.
allDeclsWithArity :: [DeclarationWithArity]
allDeclsWithArity = (convert <$> typedDecls) <> [divDecl, funToPafDecl, applyDecl]
  where
    convert (ident, t) = (ident, calcArity t)

    calcArity :: Type -> Arity
    calcArity = hylo length getParams

    getParams :: Type -> ListF Type Type
    getParams (TFun pT retT) = Cons pT retT
    getParams _ = Nil

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

divDecl :: DeclarationWithArity
divDecl = ("miniml_div", 2)

funToPafDecl :: DeclarationWithArity
funToPafDecl = ("miniml_fun_to_paf", 2)

applyDecl :: DeclarationWithArity
applyDecl = ("miniml_apply", 2)
