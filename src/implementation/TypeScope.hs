module TypeScope where

import LangDef
import Data.Maybe

data VarList = VarList [VarElem] deriving (Eq, Show)
data VarElem = Var String VarType
             | VarScope VarList deriving (Eq, Show)

data UpdateRes = Success VarList | Fail deriving (Eq, Show)

-- Check Type and Scope
check :: [Stmt] -> Bool
check stmts = checkTypeScope stmts (VarList [])
checkTypeScope :: [Stmt] -> VarList -> Bool
checkTypeScope [] _ = True
checkTypeScope (x:y) currentList = case (update x currentList) of (Success list) -> checkTypeScope y list
                                                                  Fail -> False

update :: Stmt -> VarList -> UpdateRes
update stmt list = case stmt of VarDecStmt varDec -> updateVarDeclare varDec list
                                VarReDecStmt varReDec -> updateVarReDeclare varReDec list
                                LockStmt lock -> undefined
                                ThreadStmt thread -> undefined
                                WheStmt wheStmt -> updateWhere stmt list
                                IfStmt ifStmt -> updateIf stmt list

updateVarDeclare :: VarDec -> VarList -> UpdateRes
updateVarDeclare (VarDec varType varName _) (VarList list) = if (varExisted list varName)
  then Fail else Success (VarList (list ++ [Var varName varType]))

updateVarReDeclare :: VarReDec -> VarList -> UpdateRes
updateVarReDeclare (VarReDec varName newValue) (VarList list) = case (varExistedAndVal list varName) of
                  | Nothing - Fail
                  | otherwise = Fail

updateWhere :: Stmt -> VarList -> UpdateRes
updateWhere stmt list = undefined

updateIf :: Stmt -> VarList -> UpdateRes
updateIf stmt list = undefined

-- This function is used to build the list
varExisted :: [VarElem] -> String -> Bool
varExisted list var = isJust (varExistedAndVal list var)
varExistedAndVal :: [VarElem] -> String -> Maybe VarElem -- Current List -> Variable name -> Existed inside its scope
varExistedAndVal [] _ = Nothing
varExistedAndVal (x:y) var = case x of (VarScope _) -> next
                                       (Var name varType) -> if (name == var) then (Just (Var name varType)) else next
    where next = varExistedAndVal y var

-- Check type
getType :: Expr -> VarType
getType x = case x of (NumExp _) -> Num
                      -- (VarExp _) -> TODO: Check var type
                      (BooExp _) -> Boo
                      (NumCalc _ _ _) -> Num
                      (BooCalc _ _ _) -> Boo
                      (Cond _) -> Boo
isValidCond :: Condition -> Bool
isValidCond = undefined
isValidType :: Expr -> Bool
isValidType x = case x of (NumExp _) -> True
                          -- (VarExp _) -> TODO: Check var type
                          (BooExp _) -> True
                          (NumCalc ex1 _ ex2) -> isNum ex1 && isNum ex2
                          (Cond condition) -> isValidCond condition
                          (BooCalc ex1 _ ex2) -> isBoo ex1 && isBoo ex2
isNum :: Expr -> Bool
isNum x = (getType x) == Num
isBoo :: Expr -> Bool
isBoo x = (getType x) == Boo