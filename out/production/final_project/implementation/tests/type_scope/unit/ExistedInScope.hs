module ExistedInScope where

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

check testName varName expected = TestLabel testName (TestCase (
  assertEqual testName (existedInScope varName ((\(VarScope _ x) -> x) scope)) expected))

t1 = check "t1" "a" True
t2 = check "t2" "b" False
t3 = check "t3" "c" True

run = runTestTT $ TestList [ t1, t2, t3 ]