module VarType where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser varType in BParser.hs
-- * To run, call ':l tests/parser/unit/VarType' and call 'run'
-- ==========================================================================================================

check testName stream = TestLabel testName (TestCase (assertBool testName (isRight (parse varType "Error" stream))))

t1 = check "t1" "num"
t2 = check "t2" "bool"
t3 = check "t4" "  num    "
t4 = check "t5" "  bool    "


run = runTestTT $ TestList [ t1, t2, t3, t4 ]

