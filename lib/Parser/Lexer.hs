{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer
  ( Parser,
    sc,
    lexeme,
    symbol,
    colon,
    semicolon2,
    arrow,
    eq,
    leftPar,
    rightPar,
    unitLitP,
    boolLitP,
    intLitP,
    identifierP,
    kwLet,
    kwRec,
    kwIn,
    kwIf,
    kwThen,
    kwElse,
    kwFun,
    kwUnit,
    kwBool,
    kwInt,
  )
where

import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), Parsec, choice, many, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Trees.Common (Identifier)

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

-- | Left parenthesis parser.
leftPar :: Parser Text
leftPar = symbol "("

-- | Right parenthesis parser.
rightPar :: Parser Text
rightPar = symbol ")"

-- * Literals

-- | Unit literal parser.
unitLitP :: Parser Text
unitLitP = leftPar <* rightPar

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ kwTrue <|> False <$ kwFalse

-- | Decimal integer literal parser.
intLitP :: Parser Int64
intLitP = lexeme L.decimal -- TODO : signed, return Int64

-- * Identifiers and keywords

-- ** Identifier

identifierP :: Parser Identifier
identifierP = notReserved *> identifier
  where
    identifier = lexeme $ do
      first <- letterChar <|> char '_'
      other <- many identifierChar
      return $ pack $ first : other
    notReserved =
      notFollowedBy $
        choice [kwLet, kwRec, kwIn, kwIf, kwThen, kwElse, kwFun, kwTrue, kwFalse, kwUnit, kwBool, kwInt]

keyword :: Text -> Parser Text
keyword ident = lexeme $ string ident <* notFollowedBy identifierChar

identifierChar :: Parser Char
identifierChar = letterChar <|> char '_' <|> digitChar

-- ** Keywords

-- @let@ keyword parser.
kwLet :: Parser Text
kwLet = keyword "let"

-- @rec@ keyword parser.
kwRec :: Parser Text
kwRec = keyword "rec"

-- @in@ keyword parser.
kwIn :: Parser Text
kwIn = keyword "in"

-- @if@ keyword parser.
kwIf :: Parser Text
kwIf = keyword "if"

-- @then@ keyword parser.
kwThen :: Parser Text
kwThen = keyword "then"

-- @else@ keyword parser.
kwElse :: Parser Text
kwElse = keyword "else"

-- @fun@ keyword parser.
kwFun :: Parser Text
kwFun = keyword "fun"

-- | @true@ keyword parser.
kwTrue :: Parser Text
kwTrue = keyword "true"

-- | @false@ keyword parser.
kwFalse :: Parser Text
kwFalse = keyword "false"

-- | @unit@ keyword parser.
kwUnit :: Parser Text
kwUnit = keyword "unit"

-- | @bool@ keyword parser.
kwBool :: Parser Text
kwBool = keyword "bool"

-- | @int@ keyword parser.
kwInt :: Parser Text
kwInt = keyword "int"
