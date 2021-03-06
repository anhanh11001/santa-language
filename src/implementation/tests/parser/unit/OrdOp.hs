module OrdOp where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser ordOp in BParser.hs
-- * To run, call ':l tests/parser/unit/OrdOp' and call 'run'
-- ==========================================================================================================

check testName stream = TestLabel testName
                                   (TestCase (assertBool testName (isRight (parse ordOp "Error" stream))))

t1 = check "t1" " < "
t2 = check "t2" " > "
t3 = check "t3" " == "
t4 = check "t4" " <= "
t5 = check "t5" " >= "


run = runTestTT $ TestList [ t1, t2, t3, t4, t5 ]