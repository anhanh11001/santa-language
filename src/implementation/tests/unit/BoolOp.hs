module BoolOp where

import BParser
import Test.HUnit
import Data.Either

check stream = isRight (parse boolOp "Error" stream)

t1 = TestCase (assertBool "t1" (check "&&"))
t2 = TestCase (assertBool "t2" (check "||"))
t3 = TestCase (assertBool "t3" (check " && "))
t4 = TestCase (assertBool "t4" (check " || "))


