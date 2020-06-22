module Condition where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse condition "Error" stream) (Right res))

t1 = check "t1" "1 == 2" (Condition (NumExp 1) E (NumExp 2))
t2 = check "t2" "13 >= 4" (Condition (NumExp 13) ME (NumExp 4))
t3 = check "t3" "a <= b" (Condition (VarExp "a") LE (VarExp "b"))
t4 = check "t4" "true == false" (Condition (BooExp True) E (BooExp False))
t5 = check "t5" "(1) < (2)" (Condition (Parens (NumExp 1)) L (Parens (NumExp 2)))


