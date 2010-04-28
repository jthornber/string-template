{-# LANGUAGE TypeSynonymInstances #-}
module Text.StringTemplate.Interpreter
       where

import Control.Applicative
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, State)

import Text.StringTemplate.Attribute
import Text.StringTemplate.ByteCode

----------------------------------------------------------------
-- FIXME: interpreter needs a locale for the renders
type Value = Maybe Attribute

showValue :: Value -> String
showValue Nothing = ""
showValue (Just a) = showAttr a

showAttr :: Attribute -> String
showAttr (AString s) = s
showAttr (AList xs) = concatMap showAttr xs
showAttr (AProp m) = "<properties here>" -- FIXME: finish

data VMFrame = VMFrame
    { halted :: Bool            -- FIXME: I don't think this should be here
    , attrs :: Map String Attribute
    , programCounter :: Int
    , instructions :: Code
    , stack :: [Value]
    , passThrough :: Bool
    }

newFrame :: Code -> [(String, Attribute)] -> VMFrame
newFrame c a = VMFrame { halted = False
                       , attrs = M.fromList a
                       , programCounter = 0
                       , instructions = c
                       , stack = []
                       , passThrough = False
                       }

data VMContext = VMContext
    { frames :: [VMFrame]
    , results :: [String]
    }

-- FIXME: Use WriterT for output
type VM = State VMContext

instance Applicative VM where
    pure = return
    fn <*> fa = unwrapMonad (WrapMonad fn <*> WrapMonad fa)

----------------------------------------------------------------
-- Microcode
topFrame :: VM VMFrame
topFrame = (head . frames) <$> get

modifyTopFrame :: (VMFrame -> VMFrame) -> VM ()
modifyTopFrame fn = do
  c <- get
  let fs = frames c
  put $ c { frames = (fn . head $ fs) : tail fs }

getInstruction :: VM Instruction
getInstruction = do
  f <- topFrame
  let c = instructions f
  return . S.index c . programCounter $ f

next :: VM ()
next = modifyTopFrame $ \s -> s { programCounter = succ . programCounter $ s }

incPC :: Int -> VM ()
incPC pc = modifyTopFrame $ \s -> s { programCounter = programCounter s + pc }

isHalted :: VM Bool
isHalted = halted' <$> topFrame
    where
      halted' f = programCounter f >= (S.length . instructions $ f)

push :: Value -> VM ()
push v = modifyTopFrame $ \s -> s { stack = v : stack s }

peek :: VM Value
peek = do
  (head . stack) <$> topFrame

pop :: VM Value
pop = do
  v <- peek
  modifyTopFrame $ \f -> f { stack = tail . stack $ f }
  return v

popPair :: VM (Value, Value)
popPair = (,) <$> pop <*> pop

popMany :: Int -> VM [Value]
popMany 0 = return []
popMany n = (:) <$> pop <*> popMany (n - 1)

lookupFull :: String -> VM Value
lookupFull nm = (lookup' . frames) <$> get
    where
      lookup' [] = Nothing
      lookup' (f:fs) = maybe (lookup' fs) Just (M.lookup nm (attrs f))

lookupLocal :: String -> VM Value
lookupLocal nm = lookup' <$> topFrame
    where
      lookup' = M.lookup nm . attrs

setAttribute :: String -> Value -> VM ()
setAttribute _ Nothing  = runtimeError "bad attribute value"
setAttribute n (Just a) = modifyTopFrame $ \f -> f { attrs = M.insert n a (attrs f) }

setPassThrough :: Bool -> VM ()
setPassThrough b = modifyTopFrame $ \f -> f { passThrough = b }

writeValue :: Value -> VM ()
writeValue v = modify $ \c -> c { results = showValue v : results c }

mapAttr :: Value -> Value -> VM ()
mapAttr attr templ = undefined

rotMap :: [Value] -> Value -> VM ()
rotMap tmpls v = undefined

-- Lots of instructions have this form
modifyTop :: (Value -> Value) -> VM ()
modifyTop fn = pop >>= push . fn

-- FIXME: add error handling to VM
runtimeError :: String -> a
runtimeError = error

getProperty :: String -> Value -> VM Value
getProperty n (Just (AProp m)) = return (M.lookup n m)
getProperty _ _ = runtimeError "bad property"
  
setSoleAttribute = undefined
pushIndentation = undefined
popIndentation = undefined
notValue = undefined
testAttribute = undefined
newTemplate = undefined
stLookup = undefined
toString = undefined

asList :: Value -> [Attribute]
asList Nothing = []
asList (Just (AList as)) = as
asList _ = runtimeError "what do we do here ?"

asString :: Value -> String
asString Nothing = ""
asString (Just a) = asString' a

asString' (AString s) = s
asString' (AList as)  = concatMap asString' as
asString' (AProp m)   = runtimeError "prop can't be a string" -- FIXME: check with java version

trunc = undefined
trim = undefined
strip = undefined
consAttr = undefined
vOr = undefined
vAnd = undefined

----------------------------------------------------------------
-- Interpreter

-- FIXME: make this return an Either, there must be error conditions
interpret :: Code -> [(String, Attribute)] -> String
interpret c attrs = concat . reverse . results . execState run $ context
    where
      run = step `untilM` isHalted
      context = VMContext { frames = [ newFrame c attrs ]
                          , results = []
                          }

-- FIXME: we need some cast operators like asString, asInt, asAttr etc.
step :: VM ()
step = do
  i <- getInstruction
  case i of
    LOAD_STR txt   -> push (Just $ AString txt)
    LOAD_ATTR txt  -> lookupFull txt >>= push
    LOAD_LOCAL txt -> lookupLocal txt >>= push
    LOAD_PROP prop -> pop >>= getProperty prop >>= push
    LOAD_PROP_IND  -> do
      (Just (AString prop)) <- pop
      o <- pop
      getProperty prop o >>= push
    STORE_ATTR nm  -> pop >>= setAttribute nm
    STORE_SOLE_ARG -> popPair >>= uncurry setSoleAttribute
    SET_PASS_THRU  -> setPassThrough True
    STORE_OPTION o -> undefined
    NEW tmpl       -> newTemplate tmpl >>= push
    NEW_IND        -> pop >>= newTemplate >>= push
    SUPER_NEW t    -> pop >>= stLookup >>= newTemplate >>= push
    WRITE          -> pop >>= writeValue
    WRITE_OPT      -> pop >>= writeValue -- FIXME: finish
    MAP            -> popPair >>= uncurry mapAttr
    ROT_MAP        -> ((,) <$> popMany 1 <*> pop) >>= uncurry rotMap -- FIXME: broken
    PAR_MAP        -> pop >>= undefined
    BR n           -> incPC (n - 1)
    BRF n          -> pop >>= \v -> if (testAttribute $ v) then return () else incPC (n - 1)
    OPTIONS        -> undefined
    LIST           -> push (Just . AList $ [])
    ADD            -> popPair >>= push . uncurry (consAttr)
    TOSTR          -> modifyTop toString
    FIRST          -> modifyTop $ Just . head . asList
    LAST           -> modifyTop $ Just . head . reverse . asList
    REST           -> modifyTop $ Just . AList . tail . asList
    TRUNC          -> modifyTop trunc
    STRIP          -> modifyTop strip
    TRIM           -> modifyTop trim
    LENGTH         -> modifyTop $ Just . AString . show . length . asList
    STRLEN         -> modifyTop $ Just . AString . show . length . asString
    REVERSE        -> modifyTop $ Just . AList . reverse . asList
    NOT            -> modifyTop $ notValue . testAttribute
    OR             -> popPair >>= push . uncurry vOr
    AND            -> popPair >>= push . uncurry vAnd
    INDENT txt     -> pushIndentation txt
    DEDENT         -> popIndentation
    NEWLINE        -> undefined
    NOOP           -> return ()
    POP            -> pop >> return ()
  next

----------------------------------------------------------------
-- Tests
run :: String -> [Instruction] -> [(String, Attribute)] -> String -> Test
run d c attrs expected = testCase d a
    where
      a = assertEqual "template output" expected actual
      actual = interpret (S.fromList $ c) attrs

interpreterTests :: Test
interpreterTests = testGroup "Interpreter tests"
                   [ run "load str 1" [LOAD_STR "hello", WRITE] [] "hello"
                   , run "load str 2"
                             [ LOAD_STR "hello"
                             , WRITE
                             , LOAD_STR ", world!"
                             , WRITE
                             ]
                             []
                             "hello, world!"

                   , run "load_attr 1"
                         [ LOAD_ATTR "foo"
                         , WRITE
                         ]
                         [ ("foo", AString "hello") ]
                         "hello"
                   , run "load_attr 2"
                         [ LOAD_ATTR "foo"
                         , WRITE
                         ]
                         []
                         ""
                   -- FIXME: add tests that have several frames

                   , run "load_local 1"
                         [ LOAD_LOCAL "foo"
                         , WRITE
                         ]
                         [ ("foo", AString "hello") ]
                         "hello"
                   , run "load_local 2"
                         [ LOAD_LOCAL "foo"
                         , WRITE
                         ]
                         []
                         ""
                         
                   , run "load_prop 1"
                         [ LOAD_ATTR "foo"
                         , LOAD_PROP "count"
                         , WRITE
                         ]
                         [ ("foo", AProp . M.fromList $ [("count", AString "hello")])
                         ]
                         "hello"
                   , run "load_prop 2" -- FIXME: should throw an exception?
                         [ LOAD_ATTR "foo"
                         , LOAD_PROP "unknown"
                         , WRITE
                         ]
                         [ ("foo", AProp . M.fromList $ [("count", AString "hello")])
                         ]
                         ""
                         
                   , run "load_prop_ind 1"
                         [ LOAD_ATTR "foo"
                         , LOAD_STR "count"
                         , LOAD_PROP_IND
                         , WRITE
                         ]
                         [ ("foo", AProp . M.fromList $ [("count", AString "hello")])
                         ]
                         "hello"
                         
                   , run "store_attr 1"
                         [ LOAD_STR "hello"
                         , STORE_ATTR "foo"
                         , LOAD_STR "barf"
                         , LOAD_ATTR "foo"
                         , WRITE
                         ]
                         []
                         "hello"
                   ]


