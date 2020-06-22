module VarReDec where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse varReDec "Error" stream) (Right res))

t1 = check "t1"
           "santa_change_gift bool isCreated = true"
           VarReDec "isCreated" (BooExp False)
           
      
t2 = check "t2"
           "santa_change_gift sum = 12 + 16"
           VarReDec "sum" (NumCalc (NumExp 12) Add (NumExp 16))

t3 = check "t3" 
           "santa_change_gift compared = (11 / 2) == 5"
           VarReDec "compared" (BooExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5))
           