module LangDef where

import Control.Applicative

data Program = Program [Stmt] deriving (Eq, Show)
data Scope = Scope [Stmt] deriving (Eq, Show)

data Stmt = VarDecStmt VarDec
          | VarReDecStmt VarReDec
          | WheStmt Where
          | IfStmt If
          | LockStmt Lock
          | ThreadStmt Thread deriving (Eq, Show)

data Thread = ThrCreate String Scope --
            | ThrStart String --
            | ThrStop String --
            deriving (Eq, Show)
data Lock = LckCreate String --
          | LckLock String --
          | LckUnlock String --
          deriving (Eq, Show)

data Func = Func String deriving (Eq, Show)

data VarDec = VarDec VarType String Expr deriving (Eq, Show) --
data VarReDec = VarReDec String Expr deriving (Eq, Show) --
data Where = Where Condition Scope deriving (Eq, Show) --
data If = IfOne Condition Scope Scope --
        | IfTwo Condition Scope --
        deriving (Eq, Show)

data Expr = NumExp Integer --
          | VarExp String --
          | StrExp String
          | BooExp Bool --
          | NumCalc Expr CalcOp Expr --
          | Cond Condition -- 
          | BooCalc Expr BoolOp Expr --
          | Parens Expr
          | FuncCall String [Expr]
          deriving (Eq, Show)

data VarType = Num | Boo | Char | Str deriving (Eq, Show) --
data Condition = Condition Expr OrdOp Expr deriving (Eq, Show) --
data BoolOp = And | Or deriving (Eq, Show) --
data OrdOp = L | M | E | LE | ME | NE deriving (Eq, Show) --
data CalcOp = Add | Sub | Mult | Div deriving (Eq, Show) --