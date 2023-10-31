module Parser.Utils where

import Parser.Lexer
import Text.Megaparsec (between, choice, optional, try)

-- * Parser Utilities

-- ** Backtracking support

choice' :: (Foldable f, Functor f) => f (Parser a) -> Parser a
choice' x = choice $ try <$> x

optional' :: Parser a -> Parser (Maybe a)
optional' = optional . try

-- ** Parenthesis

-- | Wraps the given parser with parenthesis.
parens :: Parser a -> Parser a
parens = between leftPar rightPar

manyParens :: Parser a -> Parser a
manyParens p = choice' [p, someParens p]

someParens :: Parser a -> Parser a
someParens p = parens $ choice' [p, someParens p]
