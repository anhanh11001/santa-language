module Program where

import BParser
import Test.HUnit
import Data.Either
import Text.ParserCombinators.Parsec
import LangDef

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This is the program test for the main parser. This test try to parse 4 different files in the same folder
-- * and match it with the manually written Program tree to see if the results match. The corresponding files and
-- * abstract syntax tree are shown in testList.
-- * To run, call ':l tests/parser/prog/Program' and call 'run0', 'run1', 'run2', 'run3'
-- ==========================================================================================================

run0 = check $ testList!!0
run1 = check $ testList!!1
run2 = check $ testList!!2
run3 = check $ testList!!3

testList :: [(String, Program, FilePath)]
testList = [ ("var_ex", varEx, "tests/parser/prog/var_ex.txt")
           , ("thread_ex", threadEx, "tests/parser/prog/thread_ex.txt")
           , ("flow_control_ex", flowControlEx, "tests/parser/prog/flow_control_ex.txt")
           , ("example", example, "tests/parser/prog/example.txt")
           ]

check (testName, expected, path) = do file <- readFile path
                                      let compiled = compile file
                                      let test = TestCase (assertEqual testName compiled expected)
                                      runTestTT test

varEx :: Program
varEx = Program [ VarDecStmt (VarDec Num "a" (NumExp 12))
                , VarDecStmt (VarDec Boo "b" (BooExp True))
                , VarDecStmt (VarDec Num "c" (NumCalc (VarExp "a") Mult (NumExp 24)))
                , VarDecStmt (VarDec Boo "d" (CondExp (NumExp 13) ME (NumExp 11)))
                , VarDecStmt (VarDec Boo "e" (BooCalc (VarExp "b") AndOp (VarExp "d")))
                , VarReDecStmt (VarReDec "a" (NumExp 13))
                , VarReDecStmt (VarReDec "b" (BooExp False))
                , VarReDecStmt (VarReDec "e" (VarExp "b"))
                ]

threadEx :: Program
threadEx = Program [ LockStmt (LckCreate "lock")
                   , ThreadStmt (ThrStart "seasonOne" (Scope [ LockStmt (LckLock "lock")
                                                             , LockStmt (LckUnlock "lock")
                                                             ]))
                   , ThreadStmt (ThrStart "seasonTwo" (Scope [ LockStmt (LckLock "lock")
                                                             , LockStmt (LckUnlock "lock")
                                                             ]))
                   , ThreadStmt (ThrStop "seasonTwo")
                   , ThreadStmt (ThrStop "seasonOne")
                   ]

flowControlEx :: Program
flowControlEx = Program [ VarDecStmt (VarDec Num "a" (NumExp 12))
                        , IfStmt (IfOne (CondExp (NumExp 13) L (NumExp 14))
                                        (Scope [VarReDecStmt (VarReDec "a" (NumExp 13))])
                                        (Scope [VarReDecStmt (VarReDec "a" (NumCalc (VarExp "a") Mult (NumExp 6)))]))
                        , VarDecStmt (VarDec Num "i" (NumExp 0))
                        , VarDecStmt (VarDec Num "sum" (NumExp 0))
                        , WheStmt (Where (CondExp (VarExp "i") L (NumExp 100))
                                         (Scope [ VarReDecStmt (VarReDec "sum" (NumCalc (VarExp "sum") AddOp (VarExp "i")))
                                                , VarReDecStmt (VarReDec "i" (NumCalc (VarExp "i") AddOp (NumExp 1)))
                                                ]))
                        ]

example :: Program
example = Program [ VarDecStmt (VarDec Num "a" (NumExp 12))
                  , VarDecStmt (VarDec Boo "b" (BooExp True))
                  , IfStmt (IfOne (CondExp (VarExp "a") ME (NumExp 11))
                                  (Scope [ VarDecStmt (VarDec Num "a" (NumExp 0))
                                         , WheStmt (Where (CondExp (VarExp "a") L (NumExp 5))
                                                          (Scope [ VarDecStmt (VarDec Boo "someRandomGift" (BooCalc (BooExp True) AndOp (BooExp False)))
                                                                 , VarDecStmt (VarDec Num "c" (NumExp 2))
                                                                 , VarReDecStmt (VarReDec "a" (NumCalc (VarExp "a") AddOp (NumExp 1)))
                                                                 , VarReDecStmt (VarReDec "c" (NumCalc (VarExp "c") Mult (NumExp 100)))
                                                                 ]))
                                         ])
                                  (Scope [ VarDecStmt (VarDec Num "a" (NumExp 0))
                                         , VarDecStmt (VarDec Boo "b" (BooExp False))
                                         , IfStmt (IfTwo (VarExp "b")
                                                         (Scope [ VarDecStmt (VarDec Num "c" (NumExp 22)) ]))
                                         ]))
                  , IfStmt (IfTwo (CondExp (VarExp "a") ME (NumExp 11))
                                  (Scope [ VarReDecStmt (VarReDec "a" (NumExp 0)) ]))
                  , VarDecStmt (VarDec Num "sum" (NumExp 0))
                  , LockStmt (LckCreate "sumLock")
                  , ThreadStmt (ThrStart "addToSum" (Scope [ VarDecStmt (VarDec Num "i" (NumExp 0))
                                                           , WheStmt (Where (CondExp (VarExp "i") L (NumExp 100))
                                                                            (Scope [ LockStmt (LckLock "sumLock")
                                                                                   , VarReDecStmt (VarReDec "sum" (NumCalc (VarExp "sum") AddOp (NumExp 5)))
                                                                                   , LockStmt (LckUnlock "sumLock")
                                                                                   ]))
                                                           ]))
                  , VarDecStmt (VarDec Num "i" (NumExp 0))
                  , WheStmt (Where (CondExp (VarExp "i") L (NumExp 100))
                                   (Scope [ LockStmt (LckLock "sumLock")
                                          , VarReDecStmt (VarReDec "sum" (NumCalc (VarExp "sum") AddOp (NumExp 5)))
                                          , LockStmt (LckUnlock "sumLock")
                                          ]))
                  , ThreadStmt (ThrStop "addToSum")
                  ]





































