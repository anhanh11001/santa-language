module Expr where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse expr "Error" stream) (Right res))

t1 = check "t1" "1" (NumExp 1)
t2 = check "t2" "personalName" (VarExp "personalName")
t3 = check "t3" "true" (BooExp True)
t4 = check "t4" " (a) " (Parens (VarExp "a"))
t5 = check "t5" "12 + a" (NumCalc (NumExp 12) Add (VarExp "a"))
t6 = check "t6" "x || false" (BooCalc (VarExp "x") Or (BooExp False))
t7 = check "t7" 
           "((13 - 4) >= 2) && x" 
           (BooCalc (Cond (CalcOp (NumExp 13) Sub (NumExp 4)) ME (NumExp 2)) Or (VarExp "x"))
t8 = check "t8" 
           "a && true && (1+1 != 2)" 
           (BooCalc (Var "a") And (BooCalc (BooExp True) And (Parens (BooCalc (NumCalc (NumExp 1) Add (NumExp 1)) NE (NumExp 2)))))
 