module LangDef where

import Control.Applicative

-- ==========================================================================================================
-- Data class of the language of the program
-- ==========================================================================================================

data Program = Program [Stmt] deriving (Eq, Show)
data Scope = Scope [Stmt] deriving (Eq, Show)

-- ==========================================================================================================
-- Data class of the statements that can be used in the program
-- ==========================================================================================================
data Stmt = VarDecStmt VarDec                                           -- Variable declaration
          | VarDecSpecialStmt VarDecSpecial                             -- Global variable declaration
          | VarReDecStmt VarReDec                                       -- Variable redeclaration
          | WheStmt Where                                               -- Where statement
          | IfStmt If                                                   -- If statement: Containing if {} or if {} else {}
          | LockStmt Lock                                               -- Lock statement: Containing create/lock/unlock a lock
          | ThreadStmt Thread                                           -- Thread statement: Containing start (fork) and stop (join) a thread
          | PrintStmt String                                            -- Print a variable value
          | ExitStmt                                                    -- Exit the program entirely
          deriving (Eq, Show)

-- ==========================================================================================================
-- Details on each statements that can be used in the program
-- ==========================================================================================================
data Thread = ThrStart String Scope
            | ThrStop String
            deriving (Eq, Show)

data Lock = LckCreate String
          | LckLock String
          | LckUnlock String
          deriving (Eq, Show)

data Func = Func String deriving (Eq, Show)

data VarDec = VarDec VarType String Expr deriving (Eq, Show)
data VarDecSpecial = VarDecSpecial VarType String Expr deriving (Eq, Show)

data VarReDec = VarReDec String Expr deriving (Eq, Show)

data Where = Where Expr Scope deriving (Eq, Show)

data If = IfOne Expr Scope Scope
        | IfTwo Expr Scope
        deriving (Eq, Show)

-- ==========================================================================================================
-- Data class for expressions used in the program
-- ==========================================================================================================
data Expr = NumExp Integer                                              -- Number
          | NumCalc Expr CalcOp Expr                                    -- Calculation for numbers/variables
          | VarExp String                                               -- Variable
          | StrExp String                                               -- String
          | BooExp Bool                                                 -- Boolean: True False
          | CondExp Expr OrdOp Expr                                     -- Comparison: <, >, ==, >=, <=, !=
          | BooCalc Expr BoolOp Expr                                    -- Boolean logic: &&, ||
          | Parens Expr                                                 -- Inside parentheses: (<expr>)
          | FuncCall String [Expr]
          deriving (Eq, Show)

-- ==========================================================================================================
-- Data class for operators and types
-- ==========================================================================================================
data VarType = Num | Boo | Char | Str | Lck | Thr deriving (Eq, Show)   -- Number, Boolean, Character, String, Lock, Thread
data BoolOp = AndOp | OrOp deriving (Eq, Show)                          -- &&, ||
data OrdOp = L | M | E | LE | ME | NE deriving (Eq, Show)               -- <, >, ==, <=, >=, !=
data CalcOp = AddOp | SubOp | Mult | Div deriving (Eq, Show)            -- +, -, *, /