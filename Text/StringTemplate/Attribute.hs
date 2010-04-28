module Text.StringTemplate.Attribute 
       where

import Data.Map (Map)

data Attribute = ASimple String
               | AList [Attribute]
               | AProp (Map String Attribute)
                 deriving (Show)


