{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse, fileP) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Parser.Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), many, optional, parseMaybe, sepEndBy1, some)

-- * MainSection

-- | Parser entry point
parse :: Parser a -> Text -> Maybe a
parse p = parseMaybe $ sc *> p <* eof

-- | Main Parser
fileP :: Parser [Program]
fileP = sepEndBy1 programP semicolon2

programP :: Parser Program
programP = Program <$> some statementP

-- | Global Statements Parser
statementP :: Parser Statement
statementP =
  choice'
    [ SExpr <$> exprP,
      SRecFunDecl <$> recFunP,
      SFunDecl <$> funP,
      SVarDecl <$> varP
    ]

-- ** DeclarationSection

varP :: Parser VarDecl
varP = VarDecl <$ kwLet <*> typedIdentifierP <* eq <*> blockP <* try (notFollowedBy kwIn)

funP :: Parser FunDecl
funP = FunDecl <$ kwLet <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP)

recFunP :: Parser RecFunDecl
recFunP = RecFunDecl <$ kwLet <* kwRec <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP)

-- * ExpressionSection

-- BlockExprParser

blockP :: Parser [Expr]
blockP = some exprP

-- MainExprParser

exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice'
    [ parens exprP,
      ELetRecInF <$ kwLet <* kwRec <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP) <* kwIn <*> blockP,
      ELetInF <$ kwLet <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP) <* kwIn <*> blockP,
      ELetInV <$ kwLet <*> typedIdentifierP <* eq <*> blockP <* kwIn <*> blockP,
      EValue <$> valueP,
      EIf <$ kwIf <*> exprP <* kwThen <*> blockP <* kwElse <*> blockP,
      EIdentifier <$> identifierP
    ]

-- OperationsExprTable
opsTable :: [[Operator Parser Expr]]
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

binaryL :: Text -> (Expr -> Expr -> Operations) -> Operator Parser Expr
binaryL name fun = InfixL $ (\e' e'' -> EOperations $ fun e' e'') <$ symbol name

booleanOp :: Text -> (Expr -> Expr -> BooleanOp) -> Operator Parser Expr
booleanOp name fun = binaryL name (\e' e'' -> BooleanOp $ fun e' e'')

comparisonOp :: Text -> (Expr -> Expr -> ComparisonOp) -> Operator Parser Expr
comparisonOp name fun = binaryL name (\e' e'' -> ComparisonOp $ fun e' e'')

arithmeticOp :: Text -> (Expr -> Expr -> ArithmeticOp) -> Operator Parser Expr
arithmeticOp name fun = binaryL name (\e' e'' -> ArithmeticOp $ fun e' e'')

notOp :: Operator Parser Expr
notOp = Prefix $ EOperations . NotOp <$ symbol "not"

applicationOp :: Operator Parser Expr
applicationOp = InfixL $ return $ \a b -> EApplication a b

-- * OtherParsersSection

-- IdentifierParsers

typedIdentifierP :: Parser (Identifier, Maybe Type)
typedIdentifierP = do
  choice'
    [ parens ((,) <$> identifierP <*> (optional . try) (colon *> typeP)),
      (,) <$> identifierP <*> pure Nothing
    ]

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
      TBool <$ idBool,
      TInt <$ idInt
    ]

-- ValueParsers

valueP :: Parser Value
valueP =
  choice'
    [ VBool <$> boolLitP,
      VInt <$> signedIntP,
      VFun <$> (Fun <$ kwFun <*> many typedIdentifierP <* arrow <*> blockP)
    ]
