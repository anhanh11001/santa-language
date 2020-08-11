module GetTypeV where

import TypeScope
import Test.HUnit
import LangDef
import TypeScope


-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for function getTypeV in TypeScope.hs
-- * To run, call ':l tests/type_scope/unit/GetTypeV' and call 'run'
-- ==========================================================================================================

motherScope :: VarScope
motherScope = VarScope NullScope [ ElemVar "a" Num
                                 , ElemVar "b" Boo
                                 ]
scope :: VarScope
scope = VarScope motherScope [ ElemVar "a" Boo
                             , ElemVar "c" Num
                             ]

check testName expr expectedType = TestLabel testName (TestCase (
  assertEqual testName (getTypeV expr scope) expectedType))

t1 = check "t1" (NumExp 12) Num
t2 = check "t2" (VarExp "c") Num
t3 = check "t3" (VarExp "b") Boo
t4 = check "t4" (BooExp True) Boo
t5 = check "t5" (NumCalc (VarExp "c") AddOp (NumExp 12)) Num
t6 = check "t6" (BooCalc (BooExp False) AndOp (VarExp "b")) Boo
t7 = check "t7" (CondExp (NumExp 3) LE (VarExp "c")) Boo

run = runTestTT $ TestList [ t1, t2, t3, t4, t5, t6, t7 ]
