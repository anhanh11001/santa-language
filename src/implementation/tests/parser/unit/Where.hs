module Where where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser whereP in BParser.hs
-- * To run, call ':l tests/parser/unit/Where' and call 'run'
-- ==========================================================================================================

check testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse whereP "Error" stream) (Right res)))

t1 = check "t1" 
           "santa go to factory when (duc == goodStudent) { santa change gift a = 13;}"
           (Where (CondExp (VarExp "duc") E (VarExp "goodStudent"))
                 (Scope [VarReDecStmt (VarReDec "a" (NumExp 13))]))

t2 = check "t2"
           "santa go to factory when (true) { santa change gift a = 14; santa make gift num b = 20; }"
           (Where (BooExp True) (Scope [VarReDecStmt (VarReDec "a" (NumExp 14)),VarDecStmt (VarDec Num "b" (NumExp 20))]))

t3 = check "t3"
            "santa go to factory when ((1 + 1) == 2) {}"
            (Where (CondExp (Parens (NumCalc (NumExp 1) AddOp (NumExp 1))) E (NumExp 2)) (Scope []))

run = runTestTT $ TestList [ t1 , t2 , t3 ]