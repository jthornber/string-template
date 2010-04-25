module Text.StringTemplate.Compiler
       where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence hiding (empty, reverse, length)
import qualified Data.Sequence as S
import Control.Applicative hiding (many, (<|>), optional)
import Text.Parsec

import Text.StringTemplate.ByteCode (Instruction (..), Code, Value (..))

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

----------------------------------------------------------------

data ParseState = ParseState {
      stringMap :: Map String Value
    }
type Parser = Parsec String ParseState

discard :: Parser a -> Parser ()
discard p = p >> return ()

dchar :: Char -> Parser ()
dchar = discard . lexeme . char

concatSeq :: [Seq a] -> Seq a
concatSeq = foldr (><) S.empty

intern :: String -> Parser Value
intern txt = do
  ps <- getState
  case M.lookup txt (stringMap ps) of
    Nothing ->
        let val = VString txt in
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

ldelim :: Parser ()
ldelim = dchar '<'

rdelim :: Parser ()
rdelim = dchar '>'

delimit :: Parser a -> Parser a
delimit = between ldelim rdelim

brak :: Char -> Char -> Parser a -> Parser a
brak l r = between (dchar l) (dchar r)

squares :: Parser a -> Parser a
squares = brak '[' ']'

parens :: Parser a -> Parser a
parens = brak '(' ')'

braces :: Parser a -> Parser a
braces = brak '{' '}'

literal :: String -> Parser ()
literal = discard . lexeme . string

ellipsis :: Parser ()
ellipsis = literal "..."

equals :: Parser ()
equals = dchar '='

identifier :: Parser String
identifier = lexeme ((:) <$> idStart <*> many idLetter)
    where
      idStart = from [['a'..'z'], ['A'..'Z'], "_/"]
      idLetter = from [['a'..'z'], ['A'..'Z'], ['0'..'9'], "_/"]
      from = oneOf . concat

lineEnd :: Parser ()
lineEnd = dchar '\n' <|> (char '\r' *> dchar '\n')

stString :: Parser String
stString = char '"' *> manyTill (escapedChar <|> noneOf "\"") (try $ char '"')
    where
      escapedChar = decode <$> (char '\\' *> anyChar)

      decode 'n' = '\n'
      decode 'r' = '\r'
      decode 't' = '\t'
      decode c   = c

comma :: Parser ()
comma = dchar ','

colon :: Parser ()
colon = dchar ':'

dot :: Parser ()
dot = dchar '.'

----------------------------------------------------------------
-- Parser
(|+>) :: Parser Code -> Instruction -> Parser Code
is |+> i = (|> i) <$> is

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP p q = (Left <$> p) <|> (Right <$> q)

many1Till :: Parser a -> Parser b -> Parser [a]
many1Till p e = (:) <$> p <*> manyTill p e

singleOp :: Instruction -> Parser Code
singleOp = pure . S.singleton

templateAndEOF :: Parser Code
templateAndEOF = template <* eof

template :: Parser Code
template = concatSeq <$> many1 element -- FIXME: should be many

indent :: Parser Code
indent = (S.singleton . INDENT) <$> many1 (oneOf " \t")

-- FIXME: lots more cases to go in here
element :: Parser Code
element = choice [ exprTag
                 , (><) <$> indent <*> text
                 , text
                 ]

text :: Parser Code
text = mkCode <$> many1Till anyChar ((lookAhead ldelim) <|> eof)
  where
    mkCode t = S.fromList [LOAD_STR t, WRITE]

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
ifstat = mkCode <$> (delimit (literal "if" *> parens conditional))
                <*> template
                <*> many ((,) <$> delimit (literal "elseif" *> parens conditional)
                              <*> template)
                <*> optionMaybe ((delimit (literal "else") *> template))
                <*> optionMaybe indent
                <*> delimit (literal "endif")
  where
    mkCode = undefined

conditional :: Parser Code
conditional = emitOr <$> andConditional <* literal "||" <*> andConditional
    where
      emitOr s1 s2 = (s1 >< s2) |> OR

andConditional :: Parser Code
andConditional = emitAnd <$> notConditional <* literal "&&" <*> notConditional
    where
      emitAnd s1 s2 = (s1 >< s2) |> AND

notConditional :: Parser Code
notConditional = (literal "!" *> memberExpr |+> NOT) <|> memberExpr

exprOptions :: Parser Code
exprOptions = concatSeq <$> sepBy1 option' comma

-- FIXME: default option case not handled
option' :: Parser Code
option' = mkCode <$> identifier <*> (optionMaybe (equals *> exprNoComma))
    where
      defaultOptions = [ ("anchor", "true")
                       , ("wrap", "\n")
                       ]

      mkCode :: String -> Maybe Code -> Code
      mkCode n mc =
        case lookup n defaultOptions of
          Nothing -> undefined -- parserFail $ "unknown option: " ++ n
          Just t  -> maybe (S.singleton $ LOAD_STR t) (\c -> c |> STORE_OPTION n) mc

exprNoComma :: Parser Code
exprNoComma = try (emitMap <$> memberExpr <*> (colon *> templateRef)) <|>
              memberExpr
    where
      emitMap s1 s2 = (s1 >< s2) |> MAP

expr :: Parser Code
expr = mapExpr

mapExpr :: Parser Code
mapExpr = mk <$> sepBy1 memberExpr comma
             <*> (optionMaybe (dchar ':' *> sepBy1 templateRef comma))
  where
    mk ms Nothing    = concatSeq ms
    mk ms (Just [t]) = (concatSeq ms >< t) |> MAP
    mk ms (Just ts)  = (concatSeq ms >< concatSeq ts) |> ROT_MAP

memberExpr :: Parser Code
memberExpr = mkInstr <$> callExpr <*> (many idOrMapExpr)
  where
    idOrMapExpr = char '.' *> (eitherP identifier (parens mapExpr))
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
      mk3 _ = S.singleton $ SET_PASS_THRU

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
listElement = mk1 <$> exprNoComma
  where
    mk1 e = e |> ADD

----------------------------------------------------------------

pTest :: String -> Parser Code -> String -> [Instruction] -> Test
pTest n p input expected = testCase (n ++ ": " ++ input) testFn
  where
    testFn = case runParser p (ParseState M.empty) "<test case>" input of
      Left err -> assertFailure $ "parse error: " ++ show err
      Right result -> assertEqual "" expected (toList result)
      
    toList s = case viewl s of
      EmptyL -> []
      x :< s' -> x : toList s'

trim :: String -> String
trim = reverse . tail . reverse . tail

compilerTests :: Test
compilerTests = testGroup "Compile tests" 
                [ primaryTest "foo" [LOAD_ATTR "foo"]
                , let str = "\"a string\"" in primaryTest str [LOAD_ATTR (trim str)]
                , primaryTest "\"\"" [LOAD_ATTR ""]
                  
                , callExprTest "foo" [LOAD_ATTR "foo"]
                  
                  -- FIXME: function lookup broken
                , callExprTest "foo(bar)" [LOAD_ATTR "bar", FAIL_COMPILE_TEST]
                  
                , callExprTest "foo()" [NEW "foo"]
                , callExprTest "foo(bar)" [NEW "foo", LOAD_ATTR "bar"]
                , callExprTest "super.foo()" [SUPER_NEW "foo"]
                  
                , argTest "foo = bar" [LOAD_ATTR "bar", STORE_ATTR "foo"]
                , argTest "foo" [LOAD_ATTR "foo", STORE_SOLE_ARG]
                , argTest "..." [SET_PASS_THRU]
                  
                , argsTest "foo = bar, baz = hux" [LOAD_ATTR "bar",STORE_ATTR "foo",LOAD_ATTR "hux",STORE_ATTR "baz"]
                ]

primaryTest :: String -> [Instruction] -> Test
primaryTest = pTest "primary" primary

callExprTest :: String -> [Instruction] -> Test
callExprTest = pTest "callExpr" callExpr

argTest :: String -> [Instruction] -> Test
argTest = pTest "argTest" arg

argsTest :: String -> [Instruction] -> Test
argsTest = pTest "argsTest" args