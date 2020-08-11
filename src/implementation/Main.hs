module Main where

import LangDef
import CodeGen
import TypeScope
import BParser
import Sprockell

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the main file that can run the language in simple_prog.txt file
-- * To run: compile with ':l Main' and call 'main' to run the program
-- ========================================================================================================

main = runProg "simple_prog.txt"
runProg fileName = do
  prog_stream <- readFile fileName                                        -- Get the strings stream from a file path
  let compiledAST = compile prog_stream                                   -- Get the generated tree after parsing the stream
  let typeScope = buildTypeScopeProg compiledAST                          -- Create a data structure to check for type and scope
  let scopeChecked = checkTypeScope typeScope                             -- Check for type and scope of the program
  let genInstrs = if scopeChecked then genCode compiledAST                -- Generate the instructions from program tree
                    else error "The stream doesn't pass type/scope tests"
  run genInstrs                                                           -- Run the program
--  runWithDebugger (debuggerSimplePrintAndWait myShow) genInstrs