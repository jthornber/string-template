module Text.StringTemplate.Attribute 
       where

import Data.Map (Map)

data Attribute = AString String
               | AList [Attribute]
               | AProp (Map String Attribute)
                 deriving (Show)


