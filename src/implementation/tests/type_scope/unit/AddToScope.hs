module AddToScope where

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
scope2 :: VarScope
scope2 = VarScope NullScope [ ElemVar "a" Boo
                            , ElemVar "c" Num
                            ]
check testName elem currentScope expected = TestLabel testName (TestCase (
  assertEqual testName (addToScope elem currentScope) expected))

t1 = check "t1" (ElemVar "d" Num) scope (VarScope motherScope [ElemVar "a" Boo, ElemVar "c" Num, ElemVar "d" Num])
t2 = check "t2" (ElemVar "d" Num) NullScope (VarScope NullScope [ElemVar "d" Num])
t3 = check "t3" (ElemScope scope2) scope (VarScope motherScope [ElemVar "a" Boo, ElemVar "c" Num, ElemScope scope2])

run = runTestTT $ TestList [ t1, t2, t3 ]

