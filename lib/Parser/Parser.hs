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

-- * Program Parser

-- | Parser entry point
parseProgram :: Text -> Maybe Program
parseProgram = parseMaybe $ sc *> programP <* eof

programP :: Parser Program
programP = Program <$> (many semicolon2 *> many (statementP <* many semicolon2))

statementP :: Parser Statement
statementP = choice' [StmtExpr <$> exprP, StmtUserDecl <$> userDeclP]

-- ** User Declaration Parsers

userDeclP :: Parser UserDeclaration
userDeclP = choice' [recFunDeclP, funDeclP, varDeclP]
  where
    varDeclP = DeclVar <$ kwLet <*> varSigP <* eq <*> exprP
    funDeclP = DeclFun <$ kwLet <*> identifierP <*> funP eq
    recFunDeclP = DeclRecFun <$ kwLet <* kwRec <*> identifierP <*> funP eq

    varSigP = manyParens $ (,) <$> manyParens identifierP <*> optionalTypeAnnotationP

-- ** Expression Parsers

exprP :: Parser Expression
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expression
exprTerm =
  choice'
    [ parens exprP,
      ExprLetIn <$> userDeclP <* kwIn <*> exprP,
      ExprValue <$> valueP,
      ExprIte <$ kwIf <*> exprP <* kwThen <*> exprP <* kwElse <*> exprP,
      ExprIdentifier <$> identifierP
    ]

-- ** Operation Parsers

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
      comparisonOp ">=" GeOp,
      comparisonOp ">" GtOp
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

valueP :: Parser Value
valueP =
  choice'
    [ ValUnit <$ unitLitP,
      ValBool <$> boolLitP,
      ValInt <$> intLitP,
      ValFun <$ kwFun <*> funP arrow
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
