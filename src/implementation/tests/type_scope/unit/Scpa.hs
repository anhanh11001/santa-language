module Scpa where

import TypeScope
import Test.HUnit
import LangDef
import TypeScope

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for function scpa in TypeScope.hs
-- * To run, call ':l tests/type_scope/unit/Scpa' and call 'run'
-- ==========================================================================================================

motherScope :: VarScope
motherScope = VarScope NullScope [ ElemVar "a" Num
                                 , ElemVar "b" Boo
                                 ]
childScope :: VarScope
childScope = VarScope motherScope [ ElemVar "a" Boo
                             , ElemVar "c" Num
                             ]
scope :: Scope
scope = Scope [ VarDecStmt (VarDec Num "x" (NumExp 12))
              , VarDecStmt (VarDec Boo "y" (BooExp True))
              , VarReDecStmt (VarReDec "y" (BooExp False)) ]

check testName scopeToAdd scopeAdded expected = TestLabel testName (TestCase (
  assertEqual testName (scpa scopeAdded scopeToAdd) expected))

t1 = check "t1"
           scope
           childScope
           (VarScope motherScope [ ElemVar "a" Boo
                                , ElemVar "c" Num
                                , ElemScope (VarScope (VarScope motherScope [ElemVar "a" Boo,ElemVar "c" Num]) [ElemVar "x" Num,ElemVar "y" Boo])
                                ])


run = runTestTT $ TestList [ t1 ]