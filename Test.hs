import Test.Framework (defaultMain)

import Text.StringTemplate.Compiler (compilerTests)
import Text.StringTemplate.Interpreter (interpreterTests)

main = defaultMain [ compilerTests
                   , interpreterTests
                   ]