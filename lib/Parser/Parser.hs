{-# LANGUAGE OverloadedStrings #-}

-- | Provides parser that produces [AST]("Parser.Ast").
module Parser.Parser where

import Control.Monad (liftM2, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either (lefts, rights)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import MaybeVoid (MaybeVoid, maybeToMaybeVoid)
import qualified Parser.Ast as Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), choice, eitherP, many, optional, parseMaybe, some, (<|>))

---------------------------------------------------------Parser---------------------------------------------------------

-- * Parser

-- | Parser entry point.
parse :: Text -> Maybe Ast.Program
parse = parseMaybe $ sc *> programP <* eof

--------------------------------------------------------Program---------------------------------------------------------

-- * Program (top-level) parsers

-- | Program parser.
programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  return Ast.Program {Ast.topLevelVarDecls = lefts decls, Ast.topLevelFunctionDefs = rights decls}

-- | Top-level declaration parser, it parses either var declaration or function definition.
topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP' (varDeclP <* semicolon) functionDefP

-- | Function definition parser.
functionDefP :: Parser Ast.FunctionDef
functionDefP = Ast.FunctionDef <$ kwFunc <*> identifierP <*> functionP

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions parsers

-- | Expression parser.
expressionP :: Parser Ast.Expression
expressionP = makeExprParser (liftM2 (foldl (&)) termExpressionP (many (funcCallOp <|> arrayAccessByIndexOp))) opsTable

-- | Terminal expression parser, it's terminal in terms of 'makeExprParser' parser.
termExpressionP :: Parser Ast.Expression
termExpressionP =
  choice'
    [ parens expressionP,
      Ast.ExprValue <$> valueP,
      Ast.ExprIdentifier <$> identifierP
    ]

-- | Operators table, contains all operator parsers and their fixity.
opsTable :: [[Operator Parser Ast.Expression]]
opsTable =
  [ [ unaryOp "+" Ast.UnaryPlusOp,
      unaryOp "-" Ast.UnaryMinusOp,
      unaryOp "!" Ast.NotOp
    ],
    [ binaryOp "*" Ast.MultOp,
      binaryOp "/" Ast.DivOp,
      binaryOp "%" Ast.ModOp
    ],
    [ binaryOp "+" Ast.PlusOp,
      binaryOp "-" Ast.MinusOp
    ],
    [ binaryOp "==" Ast.EqOp,
      binaryOp "!=" Ast.NeOp,
      binaryOp "<=" Ast.LeOp,
      binaryOp "<" Ast.LtOp,
      binaryOp ">=" Ast.MeOp,
      binaryOp ">" Ast.MtOp
    ],
    [binaryOp "&&" Ast.AndOp],
    [binaryOp "||" Ast.OrOp]
  ]

-- ** Operators

-- | Utility function, that takes operator symbol, binary operator constructor and gives new binary operator in return.
binaryOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
binaryOp opSym op = InfixL $ Ast.ExprBinaryOp op <$ symbol opSym

-- | Utility function, that takes operator symbol, unary operator constructor and gives new unary operator in return.
unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp opSym op = Prefix $ Ast.ExprUnaryOp op <$ symbol opSym

-- | Function call operator.
funcCallOp :: Parser (Ast.Expression -> Ast.Expression)
funcCallOp = flip Ast.ExprFuncCall <$> listedInPar expressionP

-- | Array access by index operator.
arrayAccessByIndexOp :: Parser (Ast.Expression -> Ast.Expression)
arrayAccessByIndexOp = flip Ast.ExprArrayAccessByIndex <$> brackets expressionP

---------------------------------------------------------Types----------------------------------------------------------

-- * Types parsers

-- | Type parser.
typeP :: Parser Ast.Type
typeP =
  choice'
    [ Ast.TInt <$ idInt,
      Ast.TBool <$ idBool,
      Ast.TString <$ idString,
      Ast.TArray <$> arrayTypeP,
      Ast.TFunction <$> functionTypeP,
      parens typeP
    ]

-- | Array type parser.
arrayTypeP :: Parser Ast.ArrayType
arrayTypeP = flip Ast.ArrayType <$> brackets expressionP <*> typeP

-- | Function type parser.
functionTypeP :: Parser Ast.FunctionType
functionTypeP = Ast.FunctionType <$ kwFunc <*> listedInPar typeP <*> maybeVoid typeP

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements parsers

-- | Statement parser.
statementP :: Parser Ast.Statement
statementP =
  choice'
    [ stmtReturnP,
      stmtForGoToP,
      stmtForP,
      Ast.StmtVarDecl <$> varDeclP,
      Ast.StmtIfElse <$> ifElseP,
      Ast.StmtBlock <$> blockP,
      Ast.StmtSimple <$> simpleStmtP
    ]

-- | Return statement parser.
stmtReturnP :: Parser Ast.Statement
stmtReturnP = Ast.StmtReturn <$ kwReturn <*> maybeVoid expressionP

-- | For goto statement parser.
stmtForGoToP :: Parser Ast.Statement
stmtForGoToP = Ast.StmtForGoTo <$> choice' [Ast.Break <$ kwBreak, Ast.Continue <$ kwContinue]

-- | For statement parser.
stmtForP :: Parser Ast.Statement
stmtForP = Ast.StmtFor <$> (Ast.For <$ void kwFor <*> forHead <*> blockP)
  where
    forHead =
      choice'
        [ Ast.ForHead
            <$> optional' simpleStmtP
            <* semicolon
            <*> optional' expressionP
            <* semicolon
            <*> optional' simpleStmtP,
          Ast.ForHead Nothing <$> optional' expressionP <*> pure Nothing
        ]

-- | Var declaration parser.
varDeclP :: Parser Ast.VarDecl
varDeclP = kwVar $> Ast.VarDecl <*> identifierP <*> varValueP
  where
    varValueP =
      choice'
        [ Ast.VarValue <$> optional' typeP <* symbol "=" <*> expressionP,
          Ast.DefaultedVarValue <$> typeP
        ]

-- | If-else parser.
ifElseP :: Parser Ast.IfElse
ifElseP = kwIf $> Ast.IfElse <*> optional' (simpleStmtP <* semicolon) <*> expressionP <*> blockP <*> elseP
  where
    elseP =
      choice'
        [ Ast.Elif <$ kwElse <*> ifElseP,
          Ast.Else <$ kwElse <*> blockP,
          return Ast.NoElse
        ]

-- | Block parser.
blockP :: Parser Ast.Block
blockP = braces $ catMaybes <$> many (optional' statementP <* semicolon)

-- | Simple statement parser.
simpleStmtP :: Parser Ast.SimpleStmt
simpleStmtP = choice' [stmtAssignmentP, stmtIncDecP, stmtShortVarDeclP, stmtExpressionP]

-- | Assignment statement parser.
stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP = Ast.StmtAssignment <$> lvalueP <* symbol "=" <*> expressionP

-- | Increment or decrement statement parser.
stmtIncDecP :: Parser Ast.SimpleStmt
stmtIncDecP = Ast.StmtIncDec <$> lvalueP <*> choice' [Ast.Inc <$ symbol "++", Ast.Dec <$ symbol "--"]

-- | Short var declaration statement parser.
stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierP <* symbol ":=" <*> expressionP

-- | Expression statement parser.
stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

-- | Lvalue parser.
lvalueP :: Parser Ast.Lvalue
lvalueP =
  choice'
    [ Ast.LvalArrEl <$> identifierP <*> some (brackets expressionP),
      Ast.LvalVar <$> identifierP
    ]

---------------------------------------------------------Values---------------------------------------------------------

-- * Values parsers

-- | Value parser.
valueP :: Parser Ast.Value
valueP =
  choice'
    [ Ast.ValInt <$> intLitP,
      Ast.ValBool <$> boolLitP,
      Ast.ValString <$> stringLitP,
      Ast.ValArray <$> arrayValP,
      Ast.ValFunction <$> functionValP
    ]

-- | Array value parser.
arrayValP :: Parser Ast.ArrayValue
arrayValP = Ast.ArrayValue <$> arrayTypeP <*> braces (listed expressionP)

-- | Function value parser.
functionValP :: Parser Ast.FunctionValue
functionValP = choice' [Ast.Nil <$ idNil, Ast.AnonymousFunction <$ kwFunc <*> functionP]

-- | Nameless function parser.
functionP :: Parser Ast.Function
functionP = Ast.Function <$> params <*> result <*> blockP
  where
    params = listedInPar ((,) <$> identifierP <*> typeP)
    result = maybeVoid typeP

---------------------------------------------------------Utils----------------------------------------------------------

-- * Utils

-- | Choice between elements parser with built-in backtracking support.
choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' ps = choice $ try <$> ps

-- | Combine two alternatives with built-in backtracking support.
eitherP' :: MonadParsec e s m => m a -> m b -> m (Either a b)
eitherP' leftP = eitherP $ try leftP

-- | Optional element parser with built-in backtracking support.
optional' :: (MonadParsec e s m) => m a -> m (Maybe a)
optional' = optional . try

maybeVoid :: (MonadParsec e s m) => m a -> m (MaybeVoid a)
maybeVoid = fmap maybeToMaybeVoid . optional'
