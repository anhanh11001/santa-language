module UpIsNum where

import CodeGen
import Test.HUnit
import Sprockell

check testName numbers ins expected = TestLabel testName (
  TestCase (assertEqual testName (upInsNum numbers ins) expected))

t1 = check "t1" (1,3) (Compute Add 1 3 2) (Compute Add 1 3 2)
t2 = check "t2" (1,3) (Load (DirAddr 1) 1) (Load (DirAddr 3) 1)
t3 = check "t3" (1,3) (Store 1 (DirAddr 1)) (Store 1 (DirAddr 3))
t4 = check "t4" (1,3) (Store 1 (IndAddr 1)) (Store 1 (IndAddr 1))
t5 = check "t5" (1,3) (ReadInstr (ImmValue 2)) (ReadInstr (ImmValue 2))
t6 = check "t6" (1,3) (TestAndSet (DirAddr 1)) (TestAndSet (DirAddr 3))
t7 = check "t7" (2,3) EndProg EndProg

runT = runTestTT $ TestList [ t1, t2, t3 , t4 , t5 , t6 , t7 ]
