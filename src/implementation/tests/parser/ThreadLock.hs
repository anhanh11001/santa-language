module ThreadLock where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

checkL testName stream res = TestCase (
  assertEqual testName (parse lock "Error" stream) (Right res))

checkT testName stream res = TestCase (
  assertEqual testName (parse thread "Error" stream) (Right res))
  
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

run = runTestTT $ TestList [ TestLabel "t1" t1
                           , TestLabel "t2" t2
                           , TestLabel "t3" t3
                           , TestLabel "t4" t4
                           , TestLabel "t5" t5
                           , TestLabel "t6" t6
                           ]