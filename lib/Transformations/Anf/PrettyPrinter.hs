module Transformations.Anf.PrettyPrinter (prettyPrint) where

import Control.Monad.State (MonadState (get), State, evalState, modify)
import Data.Text (unpack)
import StdLib (binOpIdentifier, unOpIdentifier)
import Transformations.Anf.Anf
import Trees.Common (Identifier' (Gen, Txt))

type IndentLevel = Int

type IndentState = State IndentLevel

prettyPrint :: Program -> String
prettyPrint (Program stmts) = prettyPrint' stmts

prettyPrint' :: [GlobalDeclaration] -> String
prettyPrint' stmts = unlines $ map (\stmt -> evalState (prettyStmt stmt) 0) stmts

prettyStmt :: GlobalDeclaration -> IndentState String
prettyStmt (GlobVarDecl name value) =
  doubleSemicolon <$> do
    val' <- prettyExpr value
    return $ unwords ["let", prettyId name, "=", val']
prettyStmt (GlobFunDecl name params body) =
  doubleSemicolon <$> do
    val' <- prettyExpr body
    return $ unwords ["let", prettyId name, unwords (prettyId <$> params), "=", val']

prettyExpr :: Expression -> IndentState String
prettyExpr (ExprAtom aexpr) = return $ prettyAtomic aexpr
prettyExpr (ExprComp cexpr) = prettyComplex cexpr
prettyExpr (ExprLetIn (ident, val) expr) = do
  modify (+ 2)
  indent <- get
  val' <- prettyExpr val
  expr' <- prettyExpr expr
  let res = unwords ["\n" <> replicate indent ' ' <> "let", prettyId ident, "=", val', "\n" <> replicate indent ' ' <> "in", expr']
  modify $ \x -> x - 2
  return res

prettyAtomic :: AtomicExpression -> String
prettyAtomic aexpr = case aexpr of
  AtomId name -> prettyId name
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomBinOp op lhs rhs -> parens (unwords [prettyAtomic lhs, unpack $ binOpIdentifier op, prettyAtomic rhs])
  AtomUnOp op x -> parens $ unpack (unOpIdentifier op) <> prettyAtomic x

prettyComplex :: ComplexExpression -> IndentState String
prettyComplex cexpr = case cexpr of
  CompApp f arg -> return $ parens $ prettyAtomic f <> " " <> prettyAtomic arg
  CompIte c t e -> do
    modify (+ 2)
    indent <- get
    let bindent = "\n" <> replicate (indent + 2) ' '
    let res = unwords ["\n" <> replicate indent ' ' <> "if", prettyAtomic c, bindent <> "then", prettyAtomic t, bindent <> "else", prettyAtomic e]
    modify $ \x -> x - 2
    return res

prettyId :: Identifier' -> String
prettyId (Txt n) = unpack n
prettyId (Gen n ident) = unpack ident <> "'" <> show n

parens :: String -> String
parens val = "(" <> val <> ")"

doubleSemicolon :: String -> String
doubleSemicolon = (<> ";;")
