module If where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse ifP "Error" stream) (Right res))

t1 = check "t1" 
           "santa_check (a == 2) then_he_do { santa_change_gift a = 13; } otherwise_he_do { santa_change_gift a = 14; }"
           IfOne (Condition (VarExp "a") E (NumExp 2))
                 Scope [VarReDecStmt (VarReDec "a" (NumExp 13))]
                 Scope [VarReDecStmt (VarReDec "a" (NumExp 14))]
           
t2 = check "t2"
           "santa_check (x + 10 != y) then_he_do { santa_make_gift x = 11; } "