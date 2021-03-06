module ThreadLock where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the unit test for parser thread and lock in BParser.hs
-- * To run, call ':l tests/parser/unit/ThreadLock' and call 'run'
-- ==========================================================================================================

checkL testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse lock "Error" stream) (Right res)))

checkT testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse thread "Error" stream) (Right res)))
  
t1 = checkL "t1" 
            "santa lock create ducLeTran"
            (LckCreate "ducLeTran")
            
t2 = checkL "t2"
            "santa lock ducLeTran"
            (LckLock "ducLeTran")
            
t3 = checkL "t3"
            "santa unlock ducLeTran"
            (LckUnlock "ducLeTran")

t4 = checkT "t5"
            "christmas start newGame {}"
            (ThrStart "newGame" (Scope []))
t5 = checkT "t6"
            "christmas stop newGame"
            (ThrStop "newGame")

run = runTestTT $ TestList [ t1, t2, t3, t4, t5 ]