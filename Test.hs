import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import HUnit

main = defaultMain tests

tests = 
    [ testGroup "Compiler" [ testCase "comp1" test_comp1
                           ]
    ]