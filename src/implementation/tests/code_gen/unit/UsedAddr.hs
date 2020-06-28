module UsedAddr where

import CodeGen
import Test.HUnit
import Sprockell

check testName ins nums = TestLabel testName (
  TestCase (assertEqual testName (usedAddr ins) nums))

t1 = check "t1" instructions [5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

runT = runTestTT $ TestList [ t1 ]

instructions = [ Load (ImmValue 10) 2
               , Store 2 (DirAddr 5)
               , Load (ImmValue 0) 2
               , Store 2 (DirAddr 6)
               , Load (ImmValue 1) 2
               , Store 2 (DirAddr 7)
               , Load (ImmValue 2) 2
               ,Store 2 (DirAddr 8)
               ,Load (DirAddr 8) 2
               ,Store 2 (DirAddr 9)
               ,Load (DirAddr 5) 2
               ,Store 2 (DirAddr 10)
               ,Load (DirAddr 9) 2
               ,Load (DirAddr 10) 3
               ,Compute LtE 2 3 4
               ,Store 4 (DirAddr 11)
               ,Load (ImmValue 1) 2
               ,Load (DirAddr 11) 3
               ,Compute Sub 2 3 3
               ,Branch 3 (Rel 28)
               ,Load (DirAddr 6) 2
               ,Store 2 (DirAddr 12)
               ,Load (DirAddr 7) 2
               ,Store 2 (DirAddr 13)
               ,Load (DirAddr 12) 2
               ,Load (DirAddr 13) 3
               ,Compute Add 2 3 4
               ,Store 4 (DirAddr 14)
               ,Load (DirAddr 7) 2
               ,Store 2 (DirAddr 15)
               ,Load (DirAddr 15) 2
               ,Store 2 (DirAddr 6)
               ,Load (DirAddr 14) 2
               ,Store 2 (DirAddr 16)
               ,Load (DirAddr 16) 2
               ,Store 2 (DirAddr 7)
               ,Load (DirAddr 8) 2
               ,Store 2 (DirAddr 17)
               ,Load (ImmValue 1) 2
               ,Store 2 (DirAddr 18)
               ,Load (DirAddr 17) 2
               ,Load (DirAddr 18) 3
               ,Compute Add 2 3 4
               ,Store 4 (DirAddr 19)
               ,Load (DirAddr 19) 2
               ,Store 2 (DirAddr 8)
               ,Jump (Rel (-38))
               ,Load (DirAddr 7) 2
               ,WriteInstr 2 (DirAddr 65536)
               ,EndProg]
