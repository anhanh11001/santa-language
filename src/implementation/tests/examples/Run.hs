module Run where

import Main

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the program test for the language. This tests try to run the language with the provided files
-- * To compile, call ':l tests/examples/Run'
-- * To run:
-- * Call 'runProg gcdFile' which implements GCD algorithm (Should return 121)
-- * Call 'runProg fibFile' which implements fibonacci algorithm (Should return 55)
-- * Call 'runProg foeFile' which implements an algorithm to solve first order equation (Should return 1 and 2)
-- * Call 'runProg peterFile' which implements peterson algorithms. (2 thread should execute task (print c = 1000))
-- * Call 'runProg bankFile' which creates 3 threads that add money to the same account (Add from 1 to 30) (Should
-- * return 435)
-- ==========================================================================================================

gcdFile = "tests/examples/gcd.txt"
fibFile = "tests/examples/fib.txt"
foeFile = "tests/examples/first_order_equation.txt"
bankFile = "tests/examples/banking.txt"
peterFile = "tests/examples/peterson.txt"
