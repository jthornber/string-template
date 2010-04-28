module Main where

import Control.Monad
import qualified Data.Map as M
import System.Environment
import System.Exit
import Text.StringTemplate.ByteCode
import Text.StringTemplate.Compiler
import Text.StringTemplate.Group
import Text.Parsec

usage :: IO ()
usage = do
  putStrLn "usage: CompileTemplate <template file>"
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) usage
  let file = head args
  txt <- readFile file
  case runParser templateAndEOF (ParseState M.empty) file txt of
    Left errs -> do
      putStrLn $ show errs
      exitWith $ ExitFailure 1
    Right code -> putStr $ ppCode code
