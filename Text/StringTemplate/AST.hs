module Text.StringTemplate.AST
       ( Dictionary (..)
       , FormalArg (..)
       , TemplateDef (..)
       , DictionaryDef (..)
       , Definition (..)
       ) where

import Text.StringTemplate.ByteCode

data Dictionary = Dictionary { pairs :: [(String, String)]
                             , defaultValue :: Maybe String
                             } deriving (Show)

data FormalArg = FormalArg String (Maybe String) deriving (Show)

data TemplateDef = TemplateDef { name :: String
                               , args :: [FormalArg]
                               , instructions :: Code
                               } deriving (Show)

data DictionaryDef = DictionaryDef String Dictionary deriving (Show)

data Definition = DTemplate TemplateDef
                | DDictionary DictionaryDef
                  deriving (Show)
