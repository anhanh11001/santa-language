module TypeScope where

import LangDef
import Data.Maybe
import Data.List

data VarScope = VarScope VarScope [ElemVar]
              | NullScope deriving (Eq, Show)
data ElemVar = ElemVar String VarType
             | ElemScope VarScope deriving (Eq, Show)

prettyTypeScope :: VarScope -> String
prettyTypeScope (VarScope _ elems) = "[" ++ (intercalate ", " (map prettyElem elems)) ++ "]"

prettyElem :: ElemVar -> String
prettyElem elem = case elem of (ElemVar varName varType) -> ((show varType) ++ ": " ++ varName)
                               (ElemScope scope) -> prettyTypeScope scope

-- Turn all mother scope to null
simplify :: VarScope -> VarScope
simplify NullScope = NullScope
simplify (VarScope _ elem) = VarScope NullScope (map simplify' elem)

simplify' :: ElemVar -> ElemVar
simplify' x = case x of (ElemVar _ _) -> x
                        (ElemScope scope) -> ElemScope $ simplify $ scope

-- Check Type and Scope

buildTypeScopeProg :: Program -> VarScope
buildTypeScopeProg (Program stmts) = buildTypeScope (VarScope NullScope []) stmts

buildTypeScope :: VarScope -> [Stmt] -> VarScope
buildTypeScope buildingScope [] = buildingScope
buildTypeScope buildingScope (x:y) = buildTypeScope (processElemToScope x buildingScope) y

processElemToScope :: Stmt -> VarScope -> VarScope
processElemToScope stmt buildingScope =
  case stmt of (VarDecStmt varDec) -> processVarDec varDec buildingScope
               (VarReDecStmt varReDec) -> processVarReDec varReDec buildingScope
               (WheStmt whereStmt) -> processWhere whereStmt buildingScope
               (IfStmt ifStmt) -> processIf ifStmt buildingScope
               (ThreadStmt _) -> buildingScope
               (LockStmt _) -> buildingScope

processIf :: If -> VarScope -> VarScope
processIf ifStmt scope =
  case ifStmt of (IfOne expr s1 s2) -> validCond expr scope (scpa (scpa scope s1) s2)
                 (IfTwo expr s) -> validCond expr scope (scpa scope s)

processWhere :: Where -> VarScope -> VarScope
processWhere (Where condition s) scope = validCond condition scope (scpa scope s)

processVarReDec :: VarReDec -> VarScope -> VarScope
processVarReDec (VarReDec varName newVal) scope = if (getType (VarExp varName) scope) == (getTypeV newVal scope)
  then scope else error ("Invalid variable type redeclaring: " ++ varName)

processVarDec :: VarDec -> VarScope -> VarScope
processVarDec (VarDec elemType elemName elemVal) (VarScope motherScope elems)
  | existedInScope elemName elems = error (elemName ++ " already existed!!") -- Check if variable already declared inside a scope
  | (getTypeV elemVal (VarScope motherScope elems)) /= elemType = error (elemName ++ " has invalid type: " ++ (show elemType)) -- Check if valid type
  | otherwise = addToScope (ElemVar elemName elemType) (VarScope motherScope elems)

-- Scope Helper
addToScope :: ElemVar -> VarScope -> VarScope
addToScope x NullScope = VarScope NullScope [x]
addToScope x (VarScope y z) = VarScope y (z ++ [x])

existedInScope :: String -> [ElemVar] -> Bool
existedInScope _ [] = False
existedInScope varName (x:y) = case x of (ElemScope _) -> existedNext
                                         (ElemVar name _) -> if (name == varName) then True else existedNext
                               where existedNext = existedInScope varName y

validCond :: Expr -> VarScope -> a -> a
validCond cond scope x | (getTypeV cond scope) /= Boo = error ("Invalid type (Require BOO): " ++ (show cond))
                       | otherwise = x

scpa :: VarScope -> Scope -> VarScope -- scope add
scpa scope (Scope stmts) = addToScope (ElemScope (buildTypeScope (VarScope scope []) stmts)) scope

-- Check type
getType :: Expr -> VarScope -> VarType
getType x currentScope = case x of (NumExp _) -> Num
                                   (VarExp varName) -> (\(ElemVar _ varType) -> varType) (findElem varName currentScope)
                                   (BooExp _) -> Boo
                                   (NumCalc _ _ _) -> Num
                                   (BooCalc _ _ _) -> Boo
                                   (CondExp _ _ _) -> Boo
getTypeV :: Expr -> VarScope -> VarType
getTypeV x scope = case x of (NumExp _) -> Num
                             (VarExp varName) -> (\(ElemVar _ varType) -> varType) (findElem varName scope)
                             (BooExp _) -> Boo
                             (NumCalc ex1 _ ex2) -> isNum ex1 (isNum ex2 Num)
                             (BooCalc ex1 _ ex2) -> isBoo ex1 (isBoo ex2 Boo)
                             (CondExp ex1 _ ex2) -> isNum ex1 (isNum ex2 Boo)
                   where isNum x = valid (getTypeV x scope) Num
                         isBoo x = valid (getTypeV x scope) Boo
                         valid type1 type2 val = if (type1 == type2) then val else error ("Incorrect type" ++ (show x))

findElem :: String -> VarScope -> ElemVar
findElem elem NullScope = error ("Cannot find elem " ++ elem)
findElem elem (VarScope NullScope []) = error ("Cannot find elem " ++ elem)
findElem elem (VarScope motherScope []) = findElem elem motherScope
findElem elem (VarScope motherScope (x:y)) =
    case x of (ElemVar name varType) -> if (elem == name) then (ElemVar name varType) else findNext
              (ElemScope _) -> findNext
    where findNext = findElem elem (VarScope motherScope y)

validType :: Expr -> VarScope -> a -> a
validType x scope value = case x of (NumExp _) -> value
                                    (VarExp _) -> value
                                    (BooExp _) -> value
                                    (NumCalc ex1 _ ex2) -> isNum ex1 (isNum ex2 value)
                                    (CondExp ex1 _ ex2) -> isNum ex1 (isNum ex2 value)
                                    (BooCalc ex1 _ ex2) -> isBoo ex1 (isBoo ex2 value)
                          where isNum x = valid (getTypeV x scope) Num
                                isBoo x = valid (getTypeV x scope) Boo
                                valid type1 type2 val = if (type1 == type2) then val else error ("Incorrect type" ++ (show x))