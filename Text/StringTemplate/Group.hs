module Text.StringTemplate.Group
       ( Dictionary (..)
       , FormalArg (..)
       , TemplateDef (..)
       , DictionaryDef (..)
       , Definition (..)
       , group
       ) where

import Control.Applicative hiding (many, optional)

import Data.Map (Map)
import qualified Data.Map as M

import Text.Parsec hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.String

import Text.StringTemplate.AST
import Text.StringTemplate.ByteCode
import Text.StringTemplate.Compiler
import Text.StringTemplate.ParserUtils

----------------------------------------------------------------

group :: Parser [Definition]
group = optional whiteSpace *> many1 (lexeme def)

def :: Parser Definition
def = (DTemplate <$> templateDef) <|> (DDictionary <$> dictDef)

-- FIXME: add region support and aliases
templateDef :: Parser TemplateDef
templateDef = do
  n <- identifier
  args <- brak '(' ')' (optionMaybe formalArgs)
  is
  txt <- string' <|> bigString
  case runParser templateAndEOF (ParseState M.empty) n txt of
    Left err   -> parserFail "bang"
    Right code -> return $ TemplateDef n (maybe [] id args) code

formalArgs :: Parser [FormalArg]
formalArgs = sepBy1 formalArg comma

formalArg :: Parser FormalArg
formalArg = FormalArg <$> identifier <*> optionMaybe (equals *> (string' <|> anonymousTemplate))

dictDef :: Parser DictionaryDef
dictDef = DictionaryDef <$> (identifier <* is) <*> dict

dict :: Parser Dictionary
dict = brak '[' ']' dictPairs

dictPairs :: Parser Dictionary
dictPairs = choice [ Dictionary <$> sepBy1 keyValuePair comma
                                <*> optionMaybe (comma *> defaultValuePair)
                   , (Dictionary [] . Just) <$> defaultValuePair
                   ]

defaultValuePair :: Parser String
defaultValuePair = string "default" *> colon *> keyValue

keyValuePair :: Parser (String, String)
keyValuePair = (,) <$> (string' <* colon) <*> keyValue

keyValue :: Parser String
keyValue = bigString <|> string' <|> anonymousTemplate <|> identifier

keywords :: [String]
keywords = ["default"]

identifier :: Parser String
identifier = do
  n <- lexeme $ many idLetter
  (if n `elem` keywords
   then parserFail $ "bad identifier: " ++ n
   else return n)
    where
      idLetter = from [['a'..'z'], ['A'..'Z'], ['0'..'9'], "_-"]
      from = oneOf . concat

string' :: Parser String
string' = char '"' *> manyTill (escapedChar <|> noneOf "\"") (try $ char '"')
    where
      escapedChar = decode <$> (char '\\' *> anyChar)

      decode 'n' = '\n'
      decode 'r' = '\r'
      decode 't' = '\t'
      decode c   = c

bigString :: Parser String
bigString = string "<<" *> restOfLine *> manyTill qChar (try $ string ">>")
  where
    qChar = anyChar             -- FIXME: finish

anonymousTemplate :: Parser String
anonymousTemplate = concat <$> (char '{' *> manyTill charOrBraces (char '}'))
  where
    charOrBraces = (many1 $ noneOf "{") <|> anonymousTemplate

comment :: Parser String
comment = comment' <?> "comment"
  where
    comment' = string "/*" *> manyTill anyChar (try $ string "*/")

lineComment :: Parser String
lineComment = comment' <?> "line comment"
  where
    comment' = string "//" *> manyTill anyChar (try (newline *> return () <|> eof))
newLine :: Parser String
newLine = ((try $ string "\r\n") <|> (string "\n"))

restOfLine :: Parser String
restOfLine = many (oneOf " \r") <* char '\n'

is :: Parser String
is = lexeme $ string "::="

