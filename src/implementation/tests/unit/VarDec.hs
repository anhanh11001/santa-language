module VarDec where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

check testName stream res = TestCase (
  assertEqual testName (parse varDec "Error" stream) (Right res))

t1 = check "t1"
           "santa_make_gift bool isCreated = false"
           (VarDec Boo "isCreated" (BooExp False))
           
t2 = check "t2"
           "santa_make_gift num sum = 12 + 16"
           (VarDec Num "sum" (NumCalc (NumExp 12) AddOp (NumExp 16)))

t3 = check "t3" 
           "santa_make_gift bool compared = 11 / 2 == 5"
           (VarDec Boo "compared" (CondExp (NumCalc (NumExp 11) Div (NumExp 2)) E (NumExp 5)))


tests = TestList [ TestLabel "t1" t1
                 , TestLabel "t2" t2
                 , TestLabel "t3" t3
                 ]