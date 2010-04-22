module Text.StringTemplate.Interpreter 
       where

import Attribute
import ByteCode
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Sequence (Seq)
import qualified Data.Sequence as S

----------------------------------------------------------------
-- FIXME: move somewhere else
data Template = Template {
      code :: InstrSeq
    } deriving (Show)

----------------------------------------------------------------
-- FIXME: interpreter needs a locale for the renders
data VMFrame = VMFrame {
      halted :: Bool            -- FIXME: I don't think this should be here

      attrs :: String -> Maybe Attr
      programCounter :: Int
      instructions :: InstrSeq
      stack :: [OperandValue]
      passThrough :: Bool
    }

newFrame :: Template -> VMFrame
newFrame = undefined

-- FIXME: Use WriterT for output
type VM = State [VMFrame]

----------------------------------------------------------------
-- Microcode
getInstruction :: VM Instruction
getInstruction = do
  s <- get
  c <- code s
  return . index c . programCounter $ s

next :: VM ()
next = modify $ \s -> s { programCounter = succ . programCounter $ s }

setPC :: Int -> VM ()
setPC pc = modify $ \s -> s { programCounter = pc }

isHalted :: VM Bool
isHalted = get >>= halted

push :: Value -> VM ()
push v = modify $ \s -> s { stack = v : stack s }

pop :: VM Value
pop i = do
  s <- get
  v = head . stack $ s
  set $ s { stack = tail . stack $ s }

popPair :: VM (Value, Value)
popPair = (,) <$> pop <*> pop

popMany :: Int -> VM [Value]
popMany 0 = return []
popMany n = (:) <$> pop <*> popMany (n - 1)

lookupFull :: String -> VM Attribute
lookupFull nm = undefined

lookupLocal :: String -> VM Attribute
lookupLocal nm = undefined

setAttribute :: Value -> Value -> VM ()
setAttribute nm templ v = undefined

setPassThrough :: Bool -> VM ()
setPassThrough b = do
  s <- get
  set $ s { passThrough = b }

writeValue :: Value -> VM ()
writeValue = undefined

mapAttr :: Value -> Value -> VM ()
mapAttr attr templ = undefined

rotMap :: [Value] -> Value -> VM ()
rotMap tmpls v = undefined

-- Lots of instructions have this form
modifyTop :: (Value -> Value) -> VM ()
modifyTop fn = pop >>= push . fn >> next

----------------------------------------------------------------
-- Interpreter

interpret :: Template -> [(String, Attribute)] -> String
interpret t attrs = concat . results . execState run $ newFrame t attrs
    where
      run = step `untilM` isHalted

-- FIXME: we need some cast operators like asString, asInt, asAttr etc.
-- FIXME: also need an Attr class
step :: VM ()
step = do
  i <- getInstruction
  case i of
    OP_LOAD_STR txt ->
        push txt >> next

    OP_LOAD_ATTR txt ->
        lookupFull txt >>= push >> next

    OP_LOAD_LOCAL txt ->
        lookupLocal txt >>= push >> next

    OP_LOAD_PROP prop ->
        lookupFull txt >>= getProperty prop >>= push >> next

    OP_LOAD_PROP_IND ->
        popPair >>= uncurry getProperty >>= push >> next

    OP_STORE_ATTR nm ->
        popPair >>= uncurry (setAttribute nm) >> next

    OP_STORE_SOLE_ARG ->
        popPair >>= uncurry setSoleAttribute >> next

    OP_SET_PASS_THRU ->
        setPassThrough True >> next

    OP_STORE_OPTION ->
        undefined >> next

    OP_NEW tmpl ->
        newTemplate tmpl >>= push >> next

    OP_NEW_IND ->
        pop >>= newTemplate >>= push >> next

    OP_SUPER_NEW ->
        pop >>= stLookup >>= newTemplate >>= push >> next

    OP_WRITE ->
        pop >>= writeValue >> next

    OP_WRITE_OPT ->
        pop >>= writeValue >> next -- FIXME: finish

    OP_MAP ->
        popPair >>= mapAttr >> next

    OP_ROT_MAP n ->
        ((,) <$> popMany <*> pop) >>= uncurry rotMap >> next

    OP_PAR_MAP n ->
        pop >>= undefined >> next

    OP_BR n ->
        setPC n

    OP_BRF pc ->
        pop >>= \v -> if (testAttribute $ v) then next else setPC n

    OP_OPTIONS ->
        undefined >> next

    OP_LIST ->
        (pure $ VList []) >>= push >> next

    OP_ADD ->
        popPair >>= push . uncurry (:) >> next

    OP_TOSTR ->
        modifyTop toString

    OP_FIRST ->
        modifyTop $ head . asList

    OP_LAST ->
        modifyTop $ head . reverse . asList

    OP_REST ->
        modifyTop $ tail . asList

    OP_TRUNC ->
        modifyTop trunc

    OP_STRIP ->
        modifyTop strip

    OP_TRIM ->
        modifyTop trim

    OP_LENGTH ->
        modifyTop $ length . asList

    OP_STRLEN ->
        modifyTop $ length . asString

    OP_REVERSE ->
        modifyTop $ reverse . asList

    OP_NOT ->
        modifyTop $ notValue . testAttribute

    OP_OR ->
        popPair >>= push . uncurry (||) >> next

    OP_AND ->
        popPair >>= push . uncurry (&&) >> next

    OP_INDENT txt ->
        pushIndentation txt >> next

    OP_DEDENT ->
        popIndentation >> next

    OP_NEWLINE ->
        undefined >> next

    OP_NOOP ->
        next

    OP_POP ->
        pop >> next

----------------------------------------------------------------
