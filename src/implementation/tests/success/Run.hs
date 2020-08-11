module Run where

import Main

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the program test for the language. This tests try to run the language with the provided files that
-- * should raise some correct output. To compile, call ':l tests/errors/Run' and call run<number>. For example,
-- * compile this file and call run1.
-- ==========================================================================================================

-- In short, just type run1 or run<n>

run1 = runProg "tests/success/calculation.txt"            -- Should raise 188, 2, 65, 25
run2 = runProg "tests/success/order.txt"                  -- Should raise 1,1,0,0,0 (True, True, True, False, False)
run3 = runProg "tests/success/loop.txt"                   -- Should raise 4950 (Sum from 0 to 100)
run4 = runProg "tests/success/ifelse.txt"                 -- Should raise 10 (Because statement x is true)
run5 = runProg "tests/success/nested_loop.txt"            -- Should raise 100 (2 nested loop of 10)
run6 = runProg "tests/success/sum_thread.txt"             -- Should raise 20

