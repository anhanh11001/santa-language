module VarType where

import BParser
import Test.HUnit
import Data.Either

--test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

check stream = isRight (parse varType "Error" stream)

t1 = TestCase (assertBool "t1" (check "num"))
t2 = TestCase (assertBool "t2" (check "bool"))
t3 = TestCase (assertBool "t3" (check "char"))
t4 = TestCase (assertBool "t4" (check "str"))
t5 = TestCase (assertBool "t5" (check "  num    "))
t6 = TestCase (assertBool "t6" (check "  bool    "))



