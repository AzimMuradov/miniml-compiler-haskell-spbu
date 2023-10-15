{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse, programP) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Parser.Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), many, parseMaybe, some)

-- * MainSection

-- | Parser entry point
parse :: Parser a -> Text -> Maybe a
parse p = parseMaybe $ sc *> p <* eof

-- | Main Parser
programP :: Parser Program
programP = Program <$> some statementP

-- | Global Statements Parser
statementP :: Parser Statement
statementP =
  choice'
    [ StmtExpr <$> exprP,
      StmtRecFunDecl <$> recFunP,
      StmtFunDecl <$> funP,
      StmtVarDecl <$> varP
    ]
    <* optional' semicolon2

-- ** DeclarationSection

varP :: Parser VarDecl
varP = VarDecl <$ kwLet <*> typedIdentifierP <* eq <*> exprP <* try (notFollowedBy kwIn)

funP :: Parser FunDecl
funP = FunDecl <$ kwLet <*> identifierP <*> (Fun <$> some typedIdentifierP <*> optionalTypeP <* eq <*> exprP)

recFunP :: Parser RecFunDecl
recFunP = RecFunDecl <$ kwLet <* kwRec <*> identifierP <*> (Fun <$> some typedIdentifierP <*> optionalTypeP <* eq <*> exprP)

-- * ExpressionSection

-- MainExprParser

exprP :: Parser Expression
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expression
exprTerm =
  choice'
    [ parens exprP,
      ExprLetRecInF <$ kwLet <* kwRec <*> identifierP <*> (Fun <$> some typedIdentifierP <*> optionalTypeP <* eq <*> exprP) <* kwIn <*> exprP,
      ExprLetInF <$ kwLet <*> identifierP <*> (Fun <$> some typedIdentifierP <*> optionalTypeP <* eq <*> exprP) <* kwIn <*> exprP,
      ExprLetInV <$ kwLet <*> typedIdentifierP <* eq <*> exprP <* kwIn <*> exprP,
      ExprValue <$> valueP,
      ExprIf <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP,
      ExprIdentifier <$> identifierP
    ]

-- OperationsExprTable
opsTable :: [[Operator Parser Expression]]
opsTable =
  [ [applicationOp],
    [arithmeticOp "*" MulOp, arithmeticOp "/" DivOp],
    [arithmeticOp "+" PlusOp, arithmeticOp "-" MinusOp],
    [ comparisonOp "=" EqOp,
      comparisonOp "<>" NeOp,
      comparisonOp "<=" LeOp,
      comparisonOp "<" LtOp,
      comparisonOp ">=" MeOp,
      comparisonOp ">" MtOp
    ],
    [booleanOp "&&" AndOp],
    [booleanOp "||" OrOp],
    [notOp]
  ]

binaryL :: Text -> (Expression -> Expression -> Operations) -> Operator Parser Expression
binaryL name fun = InfixL $ (\e' e'' -> ExprOperations $ fun e' e'') <$ symbol name

booleanOp :: Text -> (Expression -> Expression -> BooleanOp) -> Operator Parser Expression
booleanOp name fun = binaryL name (\e' e'' -> BooleanOp $ fun e' e'')

comparisonOp :: Text -> (Expression -> Expression -> ComparisonOp) -> Operator Parser Expression
comparisonOp name fun = binaryL name (\e' e'' -> ComparisonOp $ fun e' e'')

arithmeticOp :: Text -> (Expression -> Expression -> ArithmeticOp) -> Operator Parser Expression
arithmeticOp name fun = binaryL name (\e' e'' -> ArithmeticOp $ fun e' e'')

notOp :: Operator Parser Expression
notOp = Prefix $ ExprOperations . NotOp <$ symbol "not"

applicationOp :: Operator Parser Expression
applicationOp = InfixL $ return $ \a b -> ExprApplication a b

-- * OtherParsersSection

-- IdentifierParsers

typedIdentifierP :: Parser (Identifier, Maybe Type)
typedIdentifierP = do
  choice'
    [ parens ((,) <$> identifierP <*> optionalTypeP),
      (,) <$> identifierP <*> pure Nothing
    ]

-- TypeParser

optionalTypeP :: Parser (Maybe Type)
optionalTypeP = optional' (colon *> typeP)

typeP :: Parser Type
typeP =
  choice'
    [ TFun <$> typeP' <* arrow <*> typeP,
      typeP'
    ]

typeP' :: Parser Type
typeP' =
  choice'
    [ parens typeP,
      TBool <$ idBool,
      TInt <$ idInt
    ]

-- ValueParsers

valueP :: Parser Value
valueP =
  choice'
    [ ValBool <$> boolLitP,
      ValInt <$> signedIntP,
      ValFun <$> (Fun <$ kwFun <*> many typedIdentifierP <*> optionalTypeP <* arrow <*> exprP)
    ]
