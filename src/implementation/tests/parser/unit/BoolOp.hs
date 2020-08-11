module BoolOp where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser boolOp in BParser.hs
-- * To run, call ':l tests/parser/unit/BoolOp' and call 'run'
-- ==========================================================================================================

check testName stream = TestLabel testName
                                   (TestCase (assertBool testName (isRight (parse boolOp "Error" stream))))

t1 = check "t1" "&&"
t2 = check "t2" "||"
t3 = check "t3" " && "
t4 = check "t4" " || "

run = runTestTT $ TestList [ t1, t2, t3, t4 ]
