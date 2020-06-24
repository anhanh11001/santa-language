module ThreadLock where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

checkL testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse lock "Error" stream) (Right res)))

checkT testName stream res = TestLabel testName (TestCase (
  assertEqual testName (parse thread "Error" stream) (Right res)))
  
t1 = checkL "t1" 
            "santa_lock_create ducLeTran"
            (LckCreate "ducLeTran")
            
t2 = checkL "t2"
            "santa_lock ducLeTran"
            (LckLock "ducLeTran")
            
t3 = checkL "t3"
            "santa_unlock ducLeTran"
            (LckUnlock "ducLeTran")

t4 = checkT "t4"
            "christmas_create newGame {}"
            (ThrCreate "newGame" (Scope []))
t5 = checkT "t5"
            "christmas_start newGame"
            (ThrStart "newGame")
t6 = checkT "t6"
            "christmas_stop newGame"
            (ThrStop "newGame")

run = runTestTT $ TestList [ t1, t2, t3, t4, t5, t6 ]