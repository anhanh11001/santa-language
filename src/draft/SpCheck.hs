module SpCheck where

import Sprockell

divProg :: [Instruction]
divProg = [ ReadInstr numberIO,
          Receive regA,
          ReadInstr numberIO,
          Receive regB,
          Load (ImmValue 0) regC,

          -- Begin loop
          Compute Lt regA regB regD,    -- regA < regB
          Branch regD (Abs 10),
          Compute Sub regA regB regA,
          Compute Incr regC regC regC,
          Jump (Rel (-4)),

          -- End
          WriteInstr regC numberIO,
          EndProg
        ]

-- run the prog on 1 Sprockell core
main = run [divProg]

