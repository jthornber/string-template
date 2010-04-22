module Text.StringTemplate.Attribute 
       where

data Attribute = ASimple String
               | AList [Attribute]
               | AProp (String -> Maybe Attribute)


