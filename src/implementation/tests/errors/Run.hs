module Run where

import Main

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the program test for the language. This tests try to run the language with the provided files that
-- * should raise some errors. To compile, call ':l tests/errors/Run' and call run<number>. For example,
-- * compile this file and call run1.
-- ==========================================================================================================

run1 = runProg "tests/errors/invalid_type_1.txt"          -- Check if variable declaration has correct type for simple expression
run2 = runProg "tests/errors/invalid_type_2.txt"          -- Check if variable declaration has correct type for complicated expression
run3 = runProg "tests/errors/invalid_type_3.txt"          -- Check if variable redeclaration has correct type
run4 = runProg "tests/errors/var_exist_1.txt"             -- Check if variable existed in the main program
run5 = runProg "tests/errors/var_exist_2.txt"             -- Check if variable existed in the where scope
run6 = runProg "tests/errors/var_exist_3.txt"             -- Check if variable existed in the thread scope
run7 = runProg "tests/errors/memory_size_1.txt"           -- Check if memory out of space (maximum 3 additional threads)
run8 = runProg "tests/errors/memory_size_2.txt"           -- Check if memory out of space (maximum 4 shared memories)
run9 = runProg "tests/errors/memory_size_3.txt"           -- Check if memory out of space (maximum 32 local memories)
run10 = runProg "tests/errors/var_not_exist_1.txt"        -- Check if variable doesn't exist outside the if - scope
run11 = runProg "tests/errors/var_not_exist_2.txt"        -- Check if variable doesn't exist in a different thread
run12 = runProg "tests/errors/divide_zero.txt"            -- Check if divide by zero