module Where where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse whereP "Error" stream) (Right res)))

t1 = check "t1" 
           "santa_go_to_factory_when (duc == goodStudent) { santa_change_gift a = 13;}"
           (Where (CondExp (VarExp "duc") E (VarExp "goodStudent"))
                 (Scope [VarReDecStmt (VarReDec "a" (NumExp 13))]))

run = runTestTT $ TestList [ t1 ]