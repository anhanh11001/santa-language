module VarDec where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse varDec "Error" stream) (Right res))

t1 = check "t1"
           "santa_make_gift bool isCreated = false"
           VarDec Boo "isCreated" (BooExp False)
           
t2 = check "t2"
           "santa_make_gift num sum = 12 + 16"
           VarDec Num "sum" (NumCalc (NumExp 12) Add (NumExp 16))

t3 = check "t3" 
           "santa_make_gift bool compared = (11 / 2) == 5"
           VarDec Boo "compared" (BooExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5))