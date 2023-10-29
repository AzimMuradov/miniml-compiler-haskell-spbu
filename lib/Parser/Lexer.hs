{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Data.Text (Text, pack)
import Data.Void (Void)
import Parser.Ast (Identifier)
import Text.Megaparsec (MonadParsec (..), Parsec, between, choice, many, optional, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- * Basic lexer parts

-- | Parser monad.
type Parser = Parsec Void Text

-- | Space consumer, parses whitespace and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

-- | Lexeme, automatically parses trailing whitespace and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol, automatically parses trailing whitespace and comments.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- * Symbols

-- | Wraps the given parser with parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Colon parser.
colon :: Parser Text
colon = symbol ":"

-- | Double semicolon parser.
semicolon2 :: Parser Text
semicolon2 = symbol ";;"

-- | Arrow parser.
arrow :: Parser Text
arrow = symbol "->"

-- | Equality parser.
eq :: Parser Text
eq = symbol "="

-- * Literals

-- | Unit literal parser.
unitLitP :: Parser Text
unitLitP = parens ""

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ idTrue <|> False <$ idFalse

-- | Decimal integer literal parser.
intLitP :: Parser Integer
intLitP = lexeme L.decimal

-- * Identifiers and reserved

-- ** Identifier

identifierP :: Parser Identifier
identifierP =
  lexeme $
    notFollowedBy reservedP *> do
      first <- firstP
      other <- many otherP
      return $ pack $ first : other
  where
    firstP = letterChar <|> char '_'
    otherP = firstP <|> digitChar

reservedP :: Parser Text
reservedP = choice [kwLet, kwRec, kwIn, kwIf, kwThen, kwElse, kwFun, idUnit, idBool, idInt, idTrue, idFalse]

reservedP' :: Text -> Parser Text
reservedP' ident = lexeme $ string ident <* notFollowedBy (letterChar <|> char '_' <|> digitChar)

-- ** Keywords

-- @let@ keyword parser.
kwLet :: Parser Text
kwLet = reservedP' "let"

-- @rec@ keyword parser.
kwRec :: Parser Text
kwRec = reservedP' "rec"

-- @in@ keyword parser.
kwIn :: Parser Text
kwIn = reservedP' "in"

-- @if@ keyword parser.
kwIf :: Parser Text
kwIf = reservedP' "if"

-- @then@ keyword parser.
kwThen :: Parser Text
kwThen = reservedP' "then"

-- @else@ keyword parser.
kwElse :: Parser Text
kwElse = reservedP' "else"

-- @fun@ keyword parser.
kwFun :: Parser Text
kwFun = reservedP' "fun"

-- ** Predeclared identifiers

-- | @unit@ identifier parser.
idUnit :: Parser Text
idUnit = reservedP' "unit"

-- | @bool@ identifier parser.
idBool :: Parser Text
idBool = reservedP' "bool"

-- | @int@ identifier parser.
idInt :: Parser Text
idInt = reservedP' "int"

-- | @true@ identifier parser.
idTrue :: Parser Text
idTrue = reservedP' "true"

-- | @false@ identifier parser.
idFalse :: Parser Text
idFalse = reservedP' "false"

-- * Utilities

-- ** Backtracking support

choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' x = choice $ try <$> x

optional' :: (MonadParsec e s f) => f a -> f (Maybe a)
optional' = optional . try
