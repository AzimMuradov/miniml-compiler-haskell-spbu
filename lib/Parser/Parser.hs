{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser.Parser (parseProgram) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (some1)
import Data.Text (Text)
import Parser.Ast
import Parser.Lexer
import Parser.Utils
import Text.Megaparsec (MonadParsec (..), many, parseMaybe)
import Trees.Common

-- * Program Parser

-- | Parser entry point
parseProgram :: Text -> Maybe Program
parseProgram = parseMaybe $ sc *> programP <* eof

programP :: Parser Program
programP = Program <$> (many semicolon2 *> many (stmtP <* many semicolon2))

stmtP :: Parser Statement
stmtP = choice' [StmtExpr <$> exprP, StmtDecl <$> declP]

-- ** User Declaration Parsers

declP :: Parser Declaration
declP = choice' [recFunDeclP, funDeclP, varDeclP]
  where
    varDeclP = DeclVar <$ kwLet <*> varSigP <* eq <*> exprP
    funDeclP = flip DeclFun False <$ kwLet <*> identifierP <*> funP eq
    recFunDeclP = flip DeclFun True <$ kwLet <* kwRec <*> identifierP <*> funP eq

    varSigP = manyParens $ (,) <$> manyParens identifierP <*> optionalTypeAnnotationP

-- ** Expression Parsers

exprP :: Parser Expression
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expression
exprTerm =
  choice'
    [ parens exprP,
      ExprLetIn <$> declP <* kwIn <*> exprP,
      valueExprP,
      ExprIte <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP,
      ExprId <$> identifierP
    ]

-- ** Operation Parsers

opsTable :: [[Operator Parser Expression]]
opsTable =
  [ [appOp],
    [unOp "-" UnMinusOp],
    [arithOp "*" MulOp, arithOp "/" DivOp],
    [arithOp "+" PlusOp, arithOp "-" MinusOp],
    [ compOp "=" EqOp,
      compOp "<>" NeOp,
      compOp "<=" LeOp,
      compOp "<" LtOp,
      compOp ">=" GeOp,
      compOp ">" GtOp
    ],
    [boolOp "&&" AndOp],
    [boolOp "||" OrOp]
  ]

appOp :: Operator Parser Expression
appOp = InfixL $ return ExprApp

binLeftOp :: Text -> BinaryOperator -> Operator Parser Expression
binLeftOp name op = InfixL $ ExprBinOp op <$ symbol name

boolOp :: Text -> BooleanOperator -> Operator Parser Expression
boolOp name op = binLeftOp name $ BoolOp op

arithOp :: Text -> ArithmeticOperator -> Operator Parser Expression
arithOp name op = binLeftOp name $ ArithOp op

compOp :: Text -> ComparisonOperator -> Operator Parser Expression
compOp name op = binLeftOp name $ CompOp op

unOp :: Text -> UnaryOperator -> Operator Parser Expression
unOp name op = Prefix $ ExprUnOp op <$ symbol name

-- ** Type Parsers

typeP :: Parser Type
typeP =
  choice'
    [ TFun <$> primitiveOrInParensTypeP <* arrow <*> typeP,
      primitiveOrInParensTypeP
    ]
  where
    primitiveOrInParensTypeP = choice' [parens typeP, primitiveTypeP]
    primitiveTypeP =
      choice'
        [ TUnit <$ kwUnit,
          TBool <$ kwBool,
          TInt <$ kwInt
        ]

-- ** Value Parsers

valueExprP :: Parser Expression
valueExprP =
  choice'
    [ ExprVal ValUnit <$ unitLitP,
      ExprVal . ValBool <$> boolLitP,
      ExprVal . ValInt <$> intLitP,
      ExprFun <$ kwFun <*> funP arrow
    ]

-- ** Function Parser

funP :: Parser Text -> Parser Fun
funP sepSymbolP = Fun <$> some1 parameterP <*> optionalTypeAnnotationP <* sepSymbolP <*> exprP

parameterP :: Parser (Identifier, Maybe Type)
parameterP =
  choice'
    [ someParens $ (,) <$> manyParens identifierP <* colon <*> (Just <$> typeP),
      (,Nothing) <$> manyParens identifierP
    ]

optionalTypeAnnotationP :: Parser (Maybe Type)
optionalTypeAnnotationP = optional' (colon *> typeP)
