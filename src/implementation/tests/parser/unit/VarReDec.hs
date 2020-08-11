module VarReDec where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser varReDec in BParser.hs
-- * To run, call ':l tests/parser/unit/VarReDec' and call 'run'
-- ==========================================================================================================

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse varReDec "Error" stream) (Right res)))

t1 = check "t1"
           "santa change gift isCreated = true"
           (VarReDec "isCreated" (BooExp True))
           
      
t2 = check "t2"
           "santa change gift sum = 12 + 16"
           (VarReDec "sum" (NumCalc (NumExp 12) AddOp (NumExp 16)))

t3 = check "t3" 
           "santa change gift compared = 11 / 2 == 5"
           (VarReDec "compared" (CondExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5)))


run = runTestTT $ TestList [ t1, t2, t3 ]