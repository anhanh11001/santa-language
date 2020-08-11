module VarDecSpecial where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser varDecSpecial in BParser.hs
-- * To run, call ':l tests/parser/unit/VarDecSpecial' and call 'run'
-- ==========================================================================================================

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse varDecSpecial "Error" stream) (Right res)))

t1 = check "t1"
           "santa make special gift bool isCreated = false"
           (VarDecSpecial Boo "isCreated" (BooExp False))

t2 = check "t2"
           "santa make special gift num sum = 12 + 16"
           (VarDecSpecial Num "sum" (NumCalc (NumExp 12) AddOp (NumExp 16)))

t3 = check "t3"
           "santa make special gift bool compared = 11 / 2 == 5"
           (VarDecSpecial Boo "compared" (CondExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5)))


run = runTestTT $ TestList [ t1, t2, t3 ]