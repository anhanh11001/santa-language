module Run where

import LangDef
import CodeGen
import TypeScope
import BParser
import Sprockell

gcdFile = "tests/examples/gcd.txt"
fibFile = "tests/examples/fib.txt"
foeFile = "tests/examples/first_order_equation.txt"

runProg fileName = do
  prog_stream <- readFile fileName
  let compiledAST = compile prog_stream
  let scopeChecked = buildTypeScopeProg compiledAST -- If this run successfully without raising error, it means that scope checking is successful
  let typeScopePrint = prettyTypeScope scopeChecked
  let genInstrs = genCode compiledAST
  run genInstrs

