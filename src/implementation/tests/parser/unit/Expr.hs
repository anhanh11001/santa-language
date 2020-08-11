module Expr where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser expr in BParser.hs
-- * To run, call ':l tests/parser/unit/Expr' and call 'run'
-- ==========================================================================================================

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse expr "Error" stream) (Right res)))

t1 = check "t1" "1" (NumExp 1)
t2 = check "t2" "personalName" (VarExp "personalName")
t3 = check "t3" "true" (BooExp True)
t4 = check "t4" " (a) " (Parens (VarExp "a"))
t5 = check "t5" "12 + a" (NumCalc (NumExp 12) AddOp (VarExp "a"))
t6 = check "t6" "x || false" (BooCalc (VarExp "x") OrOp (BooExp False))
t7 = check "t7" 
           "(13 - 4 >= 2) && x"
           (BooCalc (Parens (CondExp (NumCalc (NumExp 13) SubOp (NumExp 4)) ME (NumExp 2))) AndOp (VarExp "x"))
t8 = check "t8" 
           "a && true && (1+1 != 2)" 
           (BooCalc (BooCalc (VarExp "a") AndOp (BooExp True)) AndOp (Parens (CondExp (NumCalc (NumExp 1) AddOp (NumExp 1)) NE (NumExp 2))))
run = runTestTT $ TestList [t1, t2, t3, t4, t5, t6, t7, t8]