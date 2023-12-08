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
  let declText = createIndent indent <> "let " <> prettyId ident <> " = " <> val'
  let exprText = createIndent indent <> "in " <> expr'
  modify $ \x -> x - 2
  return $ declText <> exprText

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
    indent <- get
    let cText = createIndent (indent + 2) <> "if " <> prettyAtomic c
    let tText = createIndent (indent + 4) <> "then " <> prettyAtomic t
    let eText = createIndent (indent + 4) <> "else " <> prettyAtomic e
    return $ cText <> tText <> eText

prettyId :: Identifier' -> String
prettyId (Txt n) = unpack n
prettyId (Gen n ident) = unpack ident <> "'" <> show n

createIndent :: Int -> String
createIndent indent = "\n" <> replicate indent ' '

parens :: String -> String
parens val = "(" <> val <> ")"

doubleSemicolon :: String -> String
doubleSemicolon = (<> ";;")
