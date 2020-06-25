module TSP where

import Sprockell

prog = [ Load (ImmValue 0) 2,
         Store 2 (DirAddr 3),
         Load (DirAddr 3) 2,
         Store 2 (DirAddr 1),
         Load (ImmValue 0) 2,
         Store 2 (DirAddr 2),
         Load (DirAddr 1) 2,
         Load (DirAddr 2) 3,
         Compute Equal 2 3 4,
         Store 4 (DirAddr 1),
         Load (ImmValue 1) 2,
         Load (DirAddr 1) 7,
         Compute Sub 2 7 7,
         Branch 7 (Rel 3),
         Load (ImmValue 100) 2,
         Store 2 (DirAddr 3),
         Load (DirAddr 3) regA,
         WriteInstr regA numberIO,
         EndProg]


main = run [prog]
