module Text.StringTemplate.Compiler
       ( ParseState (..)
       , templateAndEOF
       , compilerTests
       ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence hiding (empty, reverse, length)
import qualified Data.Sequence as S
import Control.Applicative hiding (many, (<|>), optional)
import Text.Parsec

import Text.StringTemplate.ByteCode (Instruction (..), Code)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

----------------------------------------------------------------

data ParseState = ParseState {
      stringMap :: Map String String
    }
type Parser = Parsec String ParseState

dchar :: Char -> Parser Char
dchar = lexeme . char

concatSeq :: [Seq a] -> Seq a
concatSeq = foldr (><) S.empty

-- FIXME: This really seems like overkill, how many times do we find
intern :: String -> Parser String
intern txt = do
  ps <- getState
  case M.lookup txt (stringMap ps) of
    Nothing ->
        let val = txt in
        do setState (ps { stringMap = M.insert txt val (stringMap ps)})
           return val

    Just x  -> return x

pushFrame :: [String] -> Parser ()
pushFrame = undefined

popFrame :: Parser ()
popFrame = undefined

lookupFunction :: String -> Parser Code
lookupFunction _ = return S.empty

----------------------------------------------------------------
-- Lexer

-- FIXME: don't consume '\n' or '\r' as these need to be left to spot line endings
whiteSpace :: Parser String
whiteSpace = many . oneOf $ " \t\n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

ldelim :: Char
ldelim = '<'

rdelim :: Char
rdelim = '>'

brak :: Char -> Char -> Parser a -> Parser a
brak l r = between (dchar l) (dchar r)

delimit :: Parser a -> Parser a
delimit = brak ldelim rdelim

squares :: Parser a -> Parser a
squares = brak '[' ']'

parens :: Parser a -> Parser a
parens = brak '(' ')'

braces :: Parser a -> Parser a
braces = brak '{' '}'

literal :: String -> Parser String
literal = lexeme . string

ellipsis :: Parser String
ellipsis = literal "..."

equals :: Parser Char
equals = dchar '='

keywords :: [String]
keywords = ["if", "else", "elseif", "endif", "super"]

identifier :: Parser String
identifier = do
  n <- lexeme ((:) <$> idStart <*> many idLetter)
  (if n `elem` keywords
   then parserFail $ "bad identifier: " ++ n
   else return n)
    where
      idStart = from [['a'..'z'], ['A'..'Z'], "_/"]
      idLetter = from [['a'..'z'], ['A'..'Z'], ['0'..'9'], "_/"]
      from = oneOf . concat

lineEnd :: Parser String
lineEnd = string "\n" <|> string "\r\n"

stString :: Parser String
stString = char '"' *> manyTill (escapedChar <|> noneOf "\"") (try $ char '"')
    where
      escapedChar = decode <$> (char '\\' *> anyChar)

      decode 'n' = '\n'
      decode 'r' = '\r'
      decode 't' = '\t'
      decode c   = c

comma :: Parser Char
comma = dchar ','

colon :: Parser Char
colon = dchar ':'

dot :: Parser Char
dot = dchar '.'

----------------------------------------------------------------
-- Parser
(|+>) :: Parser Code -> Instruction -> Parser Code
is |+> i = (|> i) <$> is

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP p q = (Left <$> p) <|> (Right <$> q)

singleOp :: Instruction -> Parser Code
singleOp = pure . S.singleton

templateAndEOF :: Parser Code
templateAndEOF = template <* eof

template :: Parser Code
template = concatSeq <$> many element

indent :: Parser Code
indent = (S.singleton . INDENT) <$> many1 (oneOf " \t")

element :: Parser Code
element = choice [ try $ indentable ifstat
                 , try $ indentable exprTag
                 , indentable text
                 ]
    where
      indentable p = ((><) <$> indent <*> p) <|> p

text :: Parser Code
text = mkCode <$> many1 (noneOf [ldelim])
    where
      mkCode t = S.fromList [ LOAD_STR t, WRITE ]

exprTag :: Parser Code
exprTag = delimit (mkCode <$> expr <*> optionMaybe (dchar ';' *> exprOptions))
  where
    mkCode e Nothing = e |> WRITE
    mkCode e (Just opts) = (e >< opts) |> WRITE_OPT

region :: Parser Code
region = delimit (mkCode <$> (dchar '@' *> identifier))
  where
    mkCode :: String -> Code
    mkCode _ = undefined

subTemplate :: Parser Code
subTemplate = undefined
{-
subTemplate = do
  dchar '{'
  args <- sepBy1 identifier comma
  dchar '|'
  pushFrame
  t <- template
  popFrame
  return $ t -- |> PUSH
-}

ifstat :: Parser Code
ifstat = mkCode <$> delimit (literal "if" *> parens conditional)
                <*> template
                <*> many ((,) <$> try (delimit (literal "elseif" *> parens conditional))
                              <*> template)
                <*> optionMaybe (try (delimit (literal "else") *> template))
                <*> optionMaybe indent
                <* delimit (literal "endif")
  where
    mkCode c t elifs me _ = mkIf ((c, t) : elifs) me

    mkIf [] Nothing       = S.empty
    mkIf [] (Just e)      = e
    mkIf ((c, t) : cts) e =
        let elseCode = mkIf cts e
            elseLen = S.length elseCode
            tLen = S.length t
        in
          if elseLen > 0
          then concatSeq [ c
                         , S.singleton $ BRF (tLen + 1)
                         , t
                         , S.singleton $ BR elseLen
                         , elseCode
                         ]
          else concatSeq [ c
                         , S.singleton $ BRF tLen
                         , t
                         ]

conditional :: Parser Code
conditional = condOp "||" OR andConditional

andConditional :: Parser Code
andConditional = condOp "&&" AND notConditional

condOp :: String -> Instruction -> Parser Code -> Parser Code
condOp sym instr sub = emitOp <$> sepBy1 sub (literal sym)
    where
      emitOp [] = S.empty -- can't happen
      emitOp [s] = s
      emitOp (s:ss)  = s >< (concatSeq . interleave (S.singleton instr) $ ss)

      -- similar to intersperse, but not quite
      interleave x = concatMap (\y -> [y, x])

notConditional :: Parser Code
notConditional = (literal "!" *> memberExpr |+> NOT) <|> memberExpr

exprOptions :: Parser Code
exprOptions = concatSeq <$> sepBy1 option' comma

-- FIXME: default option case not handled
option' :: Parser Code
option' = mkCode <$> identifier <*> optionMaybe (equals *> exprNoComma)
    where
      defaultOptions = [ ("anchor", "true")
                       , ("wrap", "\n")
                       ]

      mkCode :: String -> Maybe Code -> Code
      mkCode n mc =
        case lookup n defaultOptions of
          Nothing -> undefined -- parserFail $ "unknown option: " ++ n
          Just t  -> maybe (S.singleton $ LOAD_STR t) (|> STORE_OPTION n) mc

exprNoComma :: Parser Code
exprNoComma = try (emitMap <$> memberExpr <*> (colon *> templateRef)) <|>
              memberExpr
    where
      emitMap s1 s2 = (s1 >< s2) |> MAP

expr :: Parser Code
expr = mapExpr

mapExpr :: Parser Code
mapExpr = mk <$> sepBy1 memberExpr comma
             <*> optionMaybe (dchar ':' *> sepBy1 templateRef comma)
  where
    mk ms Nothing    = concatSeq ms
    mk ms (Just [t]) = (concatSeq ms >< t) |> MAP
    mk ms (Just ts)  = (concatSeq ms >< concatSeq ts) |> ROT_MAP

memberExpr :: Parser Code
memberExpr = mkInstr <$> callExpr <*> many idOrMapExpr
  where
    idOrMapExpr = char '.' *> eitherP identifier (parens mapExpr)
    mkInstr cs ms = cs >< (concatSeq . map toCode $ ms)
    toCode (Left n) = S.singleton $ LOAD_PROP n
    toCode (Right c) = c |> LOAD_PROP_IND

callExpr :: Parser Code
callExpr = choice [ try (do n <- identifier
                            e <- parens expr
                            f <- lookupFunction n
                            return $ e >< f)
                  , try (mk2 <$> optionMaybe (literal "super" *> dot)
                             <*> identifier
                             <*> parens (optionMaybe args))
                  , try (mk3 <$> (dchar '@' *> optionMaybe (literal "super" *> dot))
                             <*> identifier <* parens (optional whiteSpace))
                  , primary
                  ]
  where
    mk2 Nothing n Nothing = S.singleton $ NEW n
    mk2 Nothing n (Just as) = NEW n <| as
    mk2 (Just _) n Nothing = S.singleton $ SUPER_NEW n
    mk2 (Just _) n (Just as) = SUPER_NEW n <| as

    mk3 = undefined

primary :: Parser Code
primary = choice [ mk1 <$> (identifier <|> stString)
                 -- , subTemplate |+> NEW
                 , list
                 , mk3 <$> parens expr <*> optionMaybe (parens (optionMaybe args))
                 ]
    where
      -- FIXME: add handling for predefined attributes (eg, 'it')
      mk1 = S.singleton . LOAD_ATTR

      mk3 e Nothing = e |> TOSTR
      mk3 e (Just Nothing) = e |> TOSTR |> NEW_IND
      mk3 e (Just (Just as)) = (e |> TOSTR |> NEW_IND) >< as

args :: Parser Code
args = concatSeq <$> sepBy1 arg comma

arg :: Parser Code
arg = choice [ try (mk1 <$> (identifier <* equals) <*> exprNoComma)
             , mk2 <$> exprNoComma
             , mk3 <$> ellipsis
             ]
    where
      mk1 n e = e |> STORE_ATTR n
      mk2 e = e |> STORE_SOLE_ARG
      mk3 _ = S.singleton SET_PASS_THRU

templateRef :: Parser Code
templateRef = choice [ mk1 <$> identifier <* parens (optional whiteSpace)
                     -- , mk2 <$> subTemplate
                     , mk3 <$> parens mapExpr <* parens (optional whiteSpace)
                     ]
    where
      mk1 = S.singleton . LOAD_STR
      mk3 e = e |> TOSTR

list :: Parser Code
list = mk1 <$> squares (sepBy listElement comma)
  where
    mk1 ls = LIST <| concatSeq ls

listElement :: Parser Code
listElement = (|> ADD) <$> exprNoComma

----------------------------------------------------------------
-- Testing
pTest :: String -> Parser Code -> String -> [Instruction] -> Test
pTest n p input expected = testCase (n ++ " - " ++ input) testFn
  where
    testFn = case runParser p (ParseState M.empty) "<test case>" input of
      Left err     -> assertFailure $ "parse error: " ++ show err
      Right result -> assertEqual "" expected (toList result)

    toList s = case viewl s of
      EmptyL  -> []
      x :< s' -> x : toList s'

trim :: String -> String
trim = init . tail

compilerTests :: Test
compilerTests = testGroup "Compile tests"
                [ primaryTest "foo" [LOAD_ATTR "foo"]
                , let str = "\"a string\"" in
                  primaryTest str [LOAD_ATTR (trim str)]
                , primaryTest "\"\"" [LOAD_ATTR ""]

                , callExprTest "foo" [LOAD_ATTR "foo"]
                , callExprTest "foo(bar)" [LOAD_ATTR "bar", FAIL_COMPILE_TEST]
                , callExprTest "foo()" [NEW "foo"]
                , callExprTest "foo(bar)" [NEW "foo", LOAD_ATTR "bar"]
                , callExprTest "super.foo()" [SUPER_NEW "foo"]

                , argTest "foo = bar" [ LOAD_ATTR "bar"
                                      , STORE_ATTR "foo"
                                      ]
                , argTest "foo=bar" [ LOAD_ATTR "bar"
                                    , STORE_ATTR "foo"
                                    ]
                , argTest "foo" [LOAD_ATTR "foo", STORE_SOLE_ARG]
                , argTest "..." [SET_PASS_THRU]

                , argsTest "foo = bar, baz = hux" [ LOAD_ATTR "bar"
                                                  , STORE_ATTR "foo"
                                                  , LOAD_ATTR "hux"
                                                  , STORE_ATTR "baz"
                                                  ]
                , argsTest "foo = bar,baz = hux" [ LOAD_ATTR "bar"
                                                 , STORE_ATTR "foo"
                                                 , LOAD_ATTR "hux"
                                                 , STORE_ATTR "baz"
                                                 ]

                , memberTest "foo()" [NEW "foo"]
                , memberTest "foo().bar" [ NEW "foo"
                                         , LOAD_PROP "bar"
                                         ]
                , memberTest "foo().(bar)" [ NEW "foo"
                                           , LOAD_ATTR "bar"
                                           , LOAD_PROP_IND
                                           ]

                , mapTest "foo()" [ NEW "foo" ]
                , mapTest "foo():bar()" [ NEW "foo"
                                        , LOAD_STR "bar"
                                        , MAP
                                        ]
                , mapTest "foo:bar( ),hux(),baz()"
                          [ LOAD_ATTR "foo"
                          , LOAD_STR "bar"
                          , LOAD_STR "hux"
                          , LOAD_STR "baz"
                          , ROT_MAP
                          ]
                , mapTest "foo():bar() ,hux() , baz ( )"
                          [ NEW "foo"
                          , LOAD_STR "bar"
                          , LOAD_STR "hux"
                          , LOAD_STR "baz"
                          , ROT_MAP
                          ]

                , notTest "foo" [ LOAD_ATTR "foo" ]
                , notTest "!foo" [ LOAD_ATTR "foo"
                                 , NOT
                                 ]
                , notTest "! foo" [ LOAD_ATTR "foo"
                                  , NOT
                                  ]

                , andTest "foo" [LOAD_ATTR "foo"]
                , andTest "!foo" [LOAD_ATTR "foo", NOT]
                , andTest "foo && bar" [LOAD_ATTR "foo", LOAD_ATTR "bar", AND]
                , andTest "!foo && bar && !hux" [ LOAD_ATTR "foo"
                                                , NOT
                                                , LOAD_ATTR "bar"
                                                , AND
                                                , LOAD_ATTR "hux"
                                                , NOT
                                                , AND
                                                ]

                , condTest "foo || bar" [ LOAD_ATTR "foo"
                                        , LOAD_ATTR "bar"
                                        , OR
                                        ]
                , condTest "foo && bar || hux" [ LOAD_ATTR "foo"
                                               , LOAD_ATTR "bar"
                                               , AND
                                               , LOAD_ATTR "hux"
                                               , OR
                                               ]
                , condTest "foo || bar && hux" [ LOAD_ATTR "foo"
                                               , LOAD_ATTR "bar"
                                               , LOAD_ATTR "hux"
                                               , AND
                                               , OR
                                               ]

                , ifTest "<if(foo)>hello<endif>"
                             [ LOAD_ATTR "foo"
                             , BRF 2
                             , LOAD_STR "hello"
                             , WRITE
                             ]

                , ifTest "<if(foo)>hello<else>world!<endif>"
                             [ LOAD_ATTR "foo"
                             , BRF 3
                             , LOAD_STR "hello"
                             , WRITE
                             , BR 2
                             , LOAD_STR "world!"
                             , WRITE
                             ]

                , ifTest "<if(foo)>hello<elseif(bar)>world<else>!<endif>"
                         [ LOAD_ATTR "foo"
                         , BRF 3
                         , LOAD_STR "hello"
                         , WRITE
                         , BR 7
                         , LOAD_ATTR "bar"
                         , BRF 3
                         , LOAD_STR "world"
                         , WRITE
                         , BR 2
                         , LOAD_STR "!"
                         , WRITE
                         ]

                ]
    where
      andTest      = pTest "andTest" andConditional
      argTest      = pTest "argTest" arg
      argsTest     = pTest "argsTest" args
      callExprTest = pTest "callExpr" callExpr
      condTest     = pTest "conditional" conditional
      ifTest       = pTest "if" ifstat
      mapTest      = pTest "mapExpr" mapExpr
      memberTest   = pTest "memberExpr" memberExpr
      notTest      = pTest "notConditional" notConditional
      primaryTest  = pTest "primary" primary
