module Expr where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

check testName stream res = TestCase (
  assertEqual testName (parse expr "Error" stream) (Right res))

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
           (BooCalc (VarExp "a") AndOp (BooCalc (BooExp True) AndOp (Parens (CondExp (NumCalc (NumExp 1) AddOp (NumExp 1)) NE (NumExp 2)))))

tests = TestList [ TestLabel "t1" t1
                 , TestLabel "t2" t2
                 , TestLabel "t3" t3
                 , TestLabel "t4" t4
                 , TestLabel "t5" t5
                 , TestLabel "t6" t6
                 , TestLabel "t7" t7
                 , TestLabel "t8" t8
                 ]