module OrdOp where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec

check stream = isRight (parse ordOp "Error" stream)

t1 = TestCase (assertBool "t1" (check " < "))
t2 = TestCase (assertBool "t2" (check " > "))
t3 = TestCase (assertBool "t3" (check " == "))
t4 = TestCase (assertBool "t4" (check " <= "))
t5 = TestCase (assertBool "t5" (check " >= "))


tests = TestList [ TestLabel "t1" t1
                 , TestLabel "t2" t2
                 , TestLabel "t3" t3
                 , TestLabel "t4" t4
                 , TestLabel "t5" t5
                 ]