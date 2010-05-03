{-# LANGUAGE FlexibleContexts #-}

module Text.StringTemplate.ParserUtils
       ( dchar
       , comma
       , equals
       , colon
       , dot
       , brak
       , squares
       , parens
       , braces
       , literal
       , ellipsis
       , lexeme
       , whiteSpace
       ) where

import Control.Applicative hiding (optional, many)

import Text.Parsec hiding ((<|>))

dchar :: (Stream s m Char) => Char -> ParsecT s u m Char
dchar = lexeme . char

comma :: (Stream s m Char) => ParsecT s u m Char
comma = dchar ','

equals :: (Stream s m Char) => ParsecT s u m Char
equals = dchar '='

colon :: (Stream s m Char) => ParsecT s u m Char
colon = dchar ':'

dot :: (Stream s m Char) => ParsecT s u m Char
dot = dchar '.'

brak :: (Stream s m Char) => Char -> Char -> ParsecT s u m a -> ParsecT s u m a
brak b e p = dchar b *> p <* dchar e

squares :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
squares = brak '[' ']'

parens :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
parens = brak '(' ')'

braces :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
braces = brak '{' '}'

literal :: (Stream s m Char) => String -> ParsecT s u m String
literal = lexeme . string

ellipsis :: (Stream s m Char) => ParsecT s u m String
ellipsis = literal "..."

lexeme :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* optional whiteSpace

-- FIXME: whiteSpace is different for groups and templates ?
whiteSpace :: (Stream s m Char) => ParsecT s u m String
whiteSpace = (many (oneOf " \r\t\n")) <?> "whitespace"

