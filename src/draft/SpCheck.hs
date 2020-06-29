module SpCheck where

import Sprockell

prog :: [Instruction]
prog = [
           Branch regSprID (Rel 6)     -- target "beginLoop"
         , Load (ImmValue 13) regC
         , WriteInstr regC (DirAddr 1) -- Sprockell 1 must jump to second EndProg
         , WriteInstr regC (DirAddr 2) -- Sprockell 2 must jump to second EndProg
         , WriteInstr regC (DirAddr 3) -- Sprockell 3 must jump to second EndProg
         , Jump (Abs 12)               -- Sprockell 0 jumps to first EndProg
         -- beginLoop
         , ReadInstr (IndAddr regSprID)
         , Receive regA
         , Compute Equal regA reg0 regB
         , Branch regB (Rel (-3))
         -- endLoop
         , WriteInstr regA numberIO
         , Jump (Ind regA)

         -- 12: Sprockell 0 is sent here
         , EndProg

         -- 13: Sprockells 1, 2 and 3 are sent here
         , EndProg
       ]

p2 = [[Jump (Rel 2),EndProg,Compute Equal 1 1 2,Branch 2 (Rel 8),Load (ImmValue 100) 2,Store 2 (DirAddr 1),Load (DirAddr 1) 2,Store 2 (DirAddr 2),Load (DirAddr 2) 2,WriteInstr 2 (DirAddr 0),Jump (Rel (-9)),ReadInstr (DirAddr 0),Receive 2,Store 2 (DirAddr 0),Load (DirAddr 0) 2,WriteInstr 2 (DirAddr 65536),EndProg],[Jump (Rel 2),EndProg,Compute Equal 1 1 2,Branch 2 (Rel 8),Load (ImmValue 100) 2,Store 2 (DirAddr 1),Load (DirAddr 1) 2,Store 2 (DirAddr 2),Load (DirAddr 2) 2,WriteInstr 2 (DirAddr 0),Jump (Rel (-9)),ReadInstr (DirAddr 0),Receive 2,Store 2 (DirAddr 0),Load (DirAddr 0) 2,WriteInstr 2 (DirAddr 65536),EndProg]]
