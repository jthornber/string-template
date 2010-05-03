module Text.StringTemplate.ParserUtils
       ( dchar
       , comma
       , equals
       , colon
       , brak
       , lexeme
       , whiteSpace
       ) where

import Control.Applicative hiding (optional)

import Text.Parsec hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.String

dchar :: Char -> Parser Char
dchar c = lexeme (char c)

comma :: Parser Char
comma = dchar ','

equals :: Parser Char
equals = dchar '='

colon :: Parser Char
colon = dchar ':'

brak :: Char -> Char -> Parser a -> Parser a
brak b e p = dchar b *> p <* dchar e

lexeme :: Parser a -> Parser a
lexeme p = p <* optional whiteSpace

-- FIXME: whiteSpace is different for groups and templates ?
whiteSpace :: Parser String
whiteSpace = (many1 (oneOf " \r\t\n")) <?> "whitespace"

