module Main where

import LangDef
import CodeGen
import TypeScope
import BParser

main = do
  prog_stream <- readFile "simple_prog.txt"
  let compiledAST = compile prog_stream
  let scopeChecked = buildTypeScopeProg compiledAST -- If this run successfully without raising error, it means that scope checking is successful
  let typeScopePrint = prettyTypeScope scopeChecked
  return compiledAST
--  return typeScopePrint -- compiled_ast