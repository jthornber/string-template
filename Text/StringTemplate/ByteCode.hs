module Text.StringTemplate.ByteCode
    ( Code
    , ppCode
    , Instruction (..)
    ) where

import Control.Applicative
import Data.Foldable
import Data.Sequence (Seq)
import Text.StringTemplate.Attribute

type Code = Seq Instruction

ppCode :: Code -> String
ppCode code = fold (showLn <$> code)
  where
    showLn = (++ "\n") . show

data Instruction = LOAD_STR String
                 | LOAD_ATTR String
                 | LOAD_LOCAL String
                 | LOAD_PROP String   -- The name of the property
                 | LOAD_PROP_IND
                 | STORE_ATTR String -- FIXME: does this take an arg ?
                 | STORE_SOLE_ARG
                 | SET_PASS_THRU
                 | STORE_OPTION String
                 | NEW String
                 | NEW_IND
                 | SUPER_NEW String
                 | WRITE
                 | WRITE_OPT
                 | MAP
                 | ROT_MAP
                 | PAR_MAP
                 | BR Int
                 | BRF Int
                 | OPTIONS
                 | LIST
                 | ADD
                 | TOSTR
                 | FIRST
                 | LAST
                 | REST
                 | TRUNC
                 | STRIP
                 | TRIM
                 | LENGTH
                 | STRLEN
                 | REVERSE
                 | NOT
                 | OR
                 | AND
                 | INDENT String
                 | DEDENT
                 | NEWLINE
                 | NOOP
                 | POP
                 | FAIL_COMPILE_TEST
                    deriving (Show, Eq)
