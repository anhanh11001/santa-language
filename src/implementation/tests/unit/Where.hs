module Where where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

check testName stream res = TestCase (
  assertEqual testName (parse whereP "Error" stream) (Right res))

t1 = check "t1" 
           "santa_go_to_factory_when (duc == goodStudent) { santa_change_gift a = 13;}"
           (Where (CondExp (VarExp "duc") E (VarExp "goodStudent"))
                 (Scope [VarReDecStmt (VarReDec "a" (NumExp 13))]))

tests = TestList [ TestLabel "t1" t1
                 ]