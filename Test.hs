import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Text.StringTemplate.Compiler

main = defaultMain tests

tests = [ compilerTests
        ]