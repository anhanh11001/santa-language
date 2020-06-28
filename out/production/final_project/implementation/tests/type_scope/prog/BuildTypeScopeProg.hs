module BuildTypeScopeProg where

import TypeScope
import Test.HUnit
import LangDef
import TypeScope
import BParser

cmpChildOnly :: VarScope -> VarScope -> Bool
cmpChildOnly (VarScope _ elem1) (VarScope _ elem2) = cmpChildOnly' elem1 elem2

cmpChildOnly' :: [ElemVar] -> [ElemVar] -> Bool
cmpChildOnly' elem1 elem2 = foldl (&&) (length elem1 == length elem2) (map f (zip elem1 elem2))
  where f ((ElemVar x1 v1), (ElemVar x2 v2)) = x1 == x2 && v1 == v2
        f ((ElemScope scp1), (ElemScope scp2)) = cmpChildOnly scp1 scp2
        f _ = False

progTypeScope :: VarScope
progTypeScope = VarScope NullScope [ ElemVar "a" Num
                                   , ElemVar "b" Boo
                                   , ElemScope (VarScope NullScope [ ElemVar "a" Num
                                                                   , ElemScope (VarScope NullScope [ElemVar "someRandomGift" Boo, ElemVar "c" Num])
                                                                   ])
                                   , ElemScope (VarScope NullScope [ ElemVar "a" Num
                                                                   , ElemVar "b" Boo
                                                                   , ElemScope (VarScope NullScope [ElemVar "c" Num])
                                                                   ])
                                   ]

run = do prog <- readFile "tests/type_scope/prog/prog.txt"
         let compiled = compile prog
         let checked = buildTypeScopeProg compiled
         let test = TestCase (assertBool "main_test" (cmpChildOnly checked progTypeScope))
         runTestTT test

