module VarDec where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser varDec in BParser.hs
-- * To run, call ':l tests/parser/unit/VarDec' and call 'run'
-- ==========================================================================================================

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse varDec "Error" stream) (Right res)))

t1 = check "t1"
           "santa make gift bool isCreated = false"
           (VarDec Boo "isCreated" (BooExp False))
           
t2 = check "t2"
           "santa make gift num sum = 12 + 16"
           (VarDec Num "sum" (NumCalc (NumExp 12) AddOp (NumExp 16)))

t3 = check "t3" 
           "santa make gift bool compared = 11 / 2 == 5"
           (VarDec Boo "compared" (CondExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5)))


run = runTestTT $ TestList [ t1, t2, t3 ]