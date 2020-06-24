module ValidType where

import TypeScope
import Test.HUnit
import LangDef
import TypeScope

motherScope :: VarScope
motherScope = VarScope NullScope [ ElemVar "a" Num
                                 , ElemVar "b" Boo
                                 ]
scope :: VarScope
scope = VarScope motherScope [ ElemVar "a" Boo
                             , ElemVar "c" Num
                             ]

check :: String -> Expr -> Test
check testName expr = TestLabel testName (TestCase (
  assertBool testName (validType expr scope True)))

t1 = check "t1" (NumExp 12)
t2 = check "t2" (VarExp "b")
t3 = check "t3" (BooExp True)
t4 = check "t4" (BooCalc (VarExp "a") AndOp (VarExp "b"))
t5 = check "t5" (NumCalc (NumExp 12) AddOp (VarExp "c"))
t6 = check "t6" (CondExp (NumExp 13) LE (VarExp "c"))
t7 = check "t7" (NumCalc (VarExp "a") AddOp (VarExp "c"))
t8 = check "t8" (NumCalc (BooExp True) SubOp (VarExp "c"))

run = runTestTT $ TestList [ t1, t2, t3, t4, t5, t6
--                            , t7 , t8 -- These are the fail test cases so expect errors when uncommenting this line and run tests
                           ]