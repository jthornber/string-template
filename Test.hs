import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.QuickCheck
import Test.HUnit

import Text.StringTemplate.Compiler (compilerTests)
import Text.StringTemplate.Interpreter (interpreterTests)

main = defaultMain [ compilerTests
                   , interpreterTests
                   ]