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

import Text.StringTemplate.ByteCode
import Text.StringTemplate.Compiler

data Dictionary = Dictionary { pairs :: [(String, String)]
                             , defaultValue :: Maybe String
                             }

data FormalArg = FormalArg String (Maybe String)

data TemplateDef = TemplateDef { name :: String
                               , args :: [FormalArg]
                               , instructions :: Code
                               }

data DictionaryDef = DictionaryDef String Dictionary

data Definition = DTemplate TemplateDef
                | DDictionary DictionaryDef

group :: Parser [Definition]
group = many1 def

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
bigString = string "<<" *> manyTill qChar (try $ string ">>")
  where
    qChar = anyChar             -- FIXME: finish

anonymousTemplate :: Parser String
anonymousTemplate = concat <$> (char '{' *> manyTill charOrBraces (char '}'))
  where
    charOrBraces = (many1 $ noneOf "{") <|> anonymousTemplate

comment :: Parser ()
comment = discard comment'
  where
    comment' = string "/*" *> manyTill anyChar (try $ string "*/")

lineComment :: Parser ()
lineComment = discard comment'
  where
    comment' = discard (string "//" *> manyTill anyChar (try (discard newline) <|> eof))

newLine :: Parser ()
newLine = ((try $ discard . string $ "\r\n") <|> (discard $ char '\n'))

whiteSpace :: Parser ()
whiteSpace = discard $ oneOf " \r\t\n"

discard :: Parser a -> Parser ()
discard p = p >> return ()

is :: Parser ()
is = discard (lexeme $ string "::=")

dchar :: Char -> Parser ()
dchar c = char c *> optional whiteSpace *> return ()

comma :: Parser ()
comma = dchar ','

equals :: Parser ()
equals = dchar '='

colon :: Parser ()
colon = dchar ':'

brak :: Char -> Char -> Parser a -> Parser a
brak b e p = dchar b *> p <* dchar e

lexeme :: Parser a -> Parser a
lexeme p = p <* optional whiteSpace