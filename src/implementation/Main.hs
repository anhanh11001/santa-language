module Main where

import LangDef
import CodeGen
import TypeScope
import BParser
import Sprockell

main = do
  prog_stream <- readFile "test_prog.txt"
  let compiledAST = compile prog_stream
  let scopeChecked = buildTypeScopeProg compiledAST -- If this run successfully without raising error, it means that scope checking is successful
  let typeScopePrint = prettyTypeScope scopeChecked
  let genInstrs = genCode compiledAST
  run genInstrs