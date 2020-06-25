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
p2 = [Load (ImmValue 0) 2,Store 2 (DirAddr 3),Load (DirAddr 3) 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Equal 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 100) 2,Store 2 (DirAddr 3),EndProg]
p3 = [Load (ImmValue 12) 2,Store 2 (DirAddr 3),Load (ImmValue 1) 2,Store 2 (DirAddr 4),Load (DirAddr 3) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute GtE 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 43),Load (ImmValue 0) 2,Store 2 (DirAddr 5),Load (DirAddr 5) 2,Store 2 (DirAddr 1),Load (ImmValue 5) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Lt 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 28),Load (ImmValue 1) 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute And 2 3 4,Store 4 (DirAddr 6),Load (ImmValue 2) 2,Store 2 (DirAddr 7),Load (DirAddr 5) 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Add 2 3 4,Store 4 (DirAddr 5),Load (DirAddr 7) 2,Store 2 (DirAddr 1),Load (ImmValue 100) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Mul 2 3 4,Store 4 (DirAddr 7),Jump (Rel (-38)),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 8),Load (ImmValue 0) 2,Store 2 (DirAddr 9),Load (DirAddr 9) 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 22) 2,Store 2 (DirAddr 10),Load (DirAddr 8) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute GtE 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 0) 2,Store 2 (DirAddr 8),Load (ImmValue 0) 2,Store 2 (DirAddr 11),EndProg]
p4 = [Load (ImmValue 12) 2,Store 2 (DirAddr 3),Load (ImmValue 1) 2,Store 2 (DirAddr 4),Load (DirAddr 3) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute GtE 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 43),Load (ImmValue 0) 2,Store 2 (DirAddr 5),Load (DirAddr 5) 2,Store 2 (DirAddr 1),Load (ImmValue 5) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Lt 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 28),Load (ImmValue 1) 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute And 2 3 4,Store 4 (DirAddr 6),Load (ImmValue 2) 2,Store 2 (DirAddr 7),Load (DirAddr 5) 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Add 2 3 4,Store 4 (DirAddr 5),Load (DirAddr 7) 2,Store 2 (DirAddr 1),Load (ImmValue 100) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Mul 2 3 4,Store 4 (DirAddr 7),Jump (Rel (-38)),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 8),Load (ImmValue 0) 2,Store 2 (DirAddr 9),Load (DirAddr 9) 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 22) 2,Store 2 (DirAddr 10),Load (DirAddr 8) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Lt 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 300) 2,Store 2 (DirAddr 8),Load (ImmValue 202) 2,Store 2 (DirAddr 11),EndProg]
p5 = [Load (ImmValue 10) 2,Store 2 (DirAddr 3),Load (ImmValue 1) 2,Store 2 (DirAddr 4),Load (DirAddr 3) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Lt 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 300) 2,Store 2 (DirAddr 3),Load (ImmValue 300) 2,Store 2 (DirAddr 3),Load (ImmValue 202) 2,Store 2 (DirAddr 5),EndProg]
p6 = [Load (ImmValue 10) 2,Store 2 (DirAddr 3),Load (ImmValue 1) 2,Store 2 (DirAddr 4),Load (DirAddr 3) 2,Store 2 (DirAddr 1),Load (ImmValue 11) 2,Store 2 (DirAddr 2),Load (DirAddr 1) 2,Load (DirAddr 2) 3,Compute Lt 2 3 4,Store 4 (DirAddr 1),Load (ImmValue 1) 2,Load (DirAddr 1) 7,Compute Sub 2 7 7,Branch 7 (Rel 3),Load (ImmValue 30) 2,Store 2 (DirAddr 3),Load (ImmValue 300) 2,Store 2 (DirAddr 3),Load (ImmValue 202) 2,Store 2 (DirAddr 5),EndProg]

main = run [p7]