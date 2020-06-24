module VarReDec where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

check testName stream res = TestCase (
  assertEqual testName (parse varReDec "Error" stream) (Right res))

t1 = check "t1"
           "santa_change_gift isCreated = true"
           (VarReDec "isCreated" (BooExp True))
           
      
t2 = check "t2"
           "santa_change_gift sum = 12 + 16"
           (VarReDec "sum" (NumCalc (NumExp 12) AddOp (NumExp 16)))

t3 = check "t3" 
           "santa_change_gift compared = 11 / 2 == 5"
           (VarReDec "compared" (CondExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5)))


run = runTestTT $ TestList [ TestLabel "t1" t1
                           , TestLabel "t2" t2
                           , TestLabel "t3" t3
                           ]