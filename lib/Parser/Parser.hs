{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parseProgram) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (some1)
import Data.Text (Text)
import Parser.Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), parseMaybe, some)

-- * MainSection

-- | Parser entry point
parseProgram :: Text -> Maybe Program
parseProgram = parseMaybe $ sc *> programP <* eof

-- | Main Parser
programP :: Parser Program
programP = Program <$> some statementP

-- | Global Statements Parser
statementP :: Parser Statement
statementP =
  choice'
    [ StmtExpr <$> exprP,
      StmtUserDecl <$> userDeclP
    ]
    <* optional' semicolon2

userDeclP :: Parser UserDeclaration
userDeclP = choice' [recFunDeclP, funDeclP, varDeclP]
  where
    varDeclP = DeclVar <$ kwLet <*> varSigP <* eq <*> exprP
    funDeclP = DeclFun <$ kwLet <*> identifierP <*> funP eq
    recFunDeclP = DeclRecFun <$ kwLet <* kwRec <*> identifierP <*> funP eq

    varSigP = parameterP

-- * ExpressionSection

-- MainExprParser

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

-- TypeParser

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
    [ ValUnit <$ unitLitP,
      ValBool <$> boolLitP,
      ValInt <$> intLitP,
      ValFun <$ kwFun <*> funP arrow
    ]

funP :: Parser Text -> Parser Fun
funP sepSymbolP = Fun <$> some1 parameterP <*> optionalTypeP <* sepSymbolP <*> exprP

parameterP :: Parser (Identifier, Maybe Type)
parameterP = manyParensP $ (,) <$> manyParensP identifierP <*> optionalTypeP
  where
    manyParensP :: Parser a -> Parser a
    manyParensP p = choice' [p, someParensP p]

    someParensP :: Parser a -> Parser a
    someParensP p = parens $ choice' [p, someParensP p]

optionalTypeP :: Parser (Maybe Type)
optionalTypeP = optional' (colon *> typeP)
