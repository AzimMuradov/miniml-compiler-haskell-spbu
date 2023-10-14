{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Control.Applicative.Combinators (between)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), Parsec, choice)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Comments
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- Space Consumers
sc :: Parser ()
sc = L.space space1 lineComment blockComment

-- Text Parsing Helpers

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Smart Choice

choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' x = choice $ try <$> x

-- Symbols

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

mlparens :: Parser a -> Parser a
mlparens = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

block :: Parser a -> Parser a
block = between kBegin kEnd

colon :: Parser Text
colon = symbol ":"

semicolon :: Parser Text
semicolon = symbol ";"

arrow :: Parser Text
arrow = symbol "->"

comma :: Parser Text
comma = symbol ","

eq :: Parser Text
eq = symbol "="

-- Reserved Parser
reservedP :: Parser Text
reservedP = choice [wTrue, wFalse, wBool, wInt, wDouble, kIf, kThen, kElse, kLet, kRec, kIn, kFun, kMeasure, kType, kBegin, kEnd]

-- Reserved Words

-- True Parser
wTrue :: Parser Text
wTrue = symbol "true"

-- False Parser
wFalse :: Parser Text
wFalse = symbol "false"

-- Bool Parser
wBool :: Parser Text
wBool = symbol "bool"

-- Int Parser
wInt :: Parser Text
wInt= symbol "int"

-- Double Parser
wDouble :: Parser Text
wDouble = symbol "double"

-- Keyword Parsers

-- ifParser
kIf :: Parser Text
kIf = symbol "if"

-- thenParser
kThen :: Parser Text
kThen = symbol "then"

-- elseParser
kElse :: Parser Text
kElse = symbol "else"

-- letParser
kLet :: Parser Text
kLet = symbol "let"

-- recParser
kRec :: Parser Text
kRec = symbol "rec"

-- inParser
kIn :: Parser Text
kIn = symbol "in"

-- inParser
kFun :: Parser Text
kFun = symbol "fun"

-- MeasureParser
kMeasure :: Parser Text
kMeasure = symbol "[<Measure>]"

-- TypeParser
kType :: Parser Text
kType = symbol "type"

-- BeginParser
kBegin :: Parser Text
kBegin = symbol "begin"

-- EndParser
kEnd :: Parser Text
kEnd = symbol "end"