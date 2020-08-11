module FindElem where

import TypeScope
import Test.HUnit
import LangDef
import TypeScope

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for function findElem in TypeScope.hs
-- * To run, call ':l tests/type_scope/unit/FindElem' and call 'run'
-- ==========================================================================================================

motherScope :: VarScope
motherScope = VarScope NullScope [ ElemVar "a" Num
                                 , ElemVar "b" Boo
                                 ]
scope :: VarScope
scope = VarScope motherScope [ ElemVar "a" Boo
                             , ElemVar "c" Num
                             ]

check :: String -> String -> ElemVar -> Test
check testName searchName expected = TestLabel testName (TestCase (
  assertEqual testName (findElem searchName scope) expected))

t1 = check "t1" "a" (ElemVar "a" Boo)
t2 = check "t2" "b" (ElemVar "b" Boo)
t3 = check "t3" "c" (ElemVar "c" Num)

run = runTestTT $ TestList [ t1, t2, t3 ]