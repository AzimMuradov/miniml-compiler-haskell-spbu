{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse, programP) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (some1)
import Data.Text (Text)
import Parser.Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), parseMaybe, some)

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
      StmtRecFunDecl <$ kwLet <* kwRec <*> identifierP <*> funP eq,
      StmtFunDecl <$ kwLet <*> identifierP <*> funP eq,
      StmtVarDecl <$ kwLet <*> typedIdentifierP <* eq <*> exprP
    ]
    <* optional' semicolon2

-- * ExpressionSection

-- MainExprParser

exprP :: Parser Expression
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expression
exprTerm =
  choice'
    [ parens exprP,
      ExprLetRecInF <$ kwLet <* kwRec <*> identifierP <*> funP eq <* kwIn <*> exprP,
      ExprLetInF <$ kwLet <*> identifierP <*> funP eq <* kwIn <*> exprP,
      ExprLetInV <$ kwLet <*> typedIdentifierP <* eq <*> exprP <* kwIn <*> exprP,
      ExprValue <$> valueP,
      ExprIf <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP,
      ExprIdentifier <$> identifierP
    ]

-- ** Operation parsers

opsTable :: [[Operator Parser Expression]]
opsTable =
  [ [applicationOp],
    [unaryOp "-" UnaryMinusOp],
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
    [booleanOp "||" OrOp]
  ]

applicationOp :: Operator Parser Expression
applicationOp = InfixL $ return ExprApplication

binaryLeftOp :: Text -> BinaryOperator -> Operator Parser Expression
binaryLeftOp name op = InfixL $ ExprBinaryOperation op <$ symbol name

booleanOp :: Text -> BooleanOperator -> Operator Parser Expression
booleanOp name op = binaryLeftOp name $ BooleanOp op

arithmeticOp :: Text -> ArithmeticOperator -> Operator Parser Expression
arithmeticOp name op = binaryLeftOp name $ ArithmeticOp op

comparisonOp :: Text -> ComparisonOperator -> Operator Parser Expression
comparisonOp name op = binaryLeftOp name $ ComparisonOp op

unaryOp :: Text -> UnaryOperator -> Operator Parser Expression
unaryOp name op = Prefix $ ExprUnaryOperation op <$ symbol name

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
      TUnit <$ idUnit,
      TBool <$ idBool,
      TInt <$ idInt
    ]

-- ValueParsers

valueP :: Parser Value
valueP =
  choice'
    [ ValUnit <$ unitP,
      ValBool <$> boolLitP,
      ValInt <$> intLitP,
      ValFun <$ kwFun <*> funP arrow
    ]

funP :: Parser Text -> Parser Fun
funP sepSymbolP = Fun <$> some1 typedIdentifierP <*> optionalTypeP <* sepSymbolP <*> exprP
