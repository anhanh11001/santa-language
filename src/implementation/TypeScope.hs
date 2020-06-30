module TypeScope where

import LangDef
import Data.Maybe
import Data.List

-- ==========================================================================================================
-- TypeScope data class/data structure to store and check whether an AST is valid
-- ==========================================================================================================
data VarScope = VarScope VarScope [ElemVar]                                                      -- A scope can have a parent scope and store a list of elements inside
              | NullScope deriving (Eq, Show)                                                    -- Empty scope. Ex: The main program has NullScope as a parent
data ElemVar = ElemVar String VarType                                                            -- Local variable (Name and type)
             | ElemSharedVar String VarType                                                      -- Global variable (Name and type)
             | ElemScope VarScope deriving (Eq, Show)                                            -- Child scope. Ex: Scopes used for if-else, where statement

-- ==========================================================================================================
-- Functions to build the type scope data structure above for the program. If it fails to build, it means that the program has failed type/scope checking
-- ==========================================================================================================

buildTypeScopeProg :: Program -> VarScope
buildTypeScopeProg (Program stmts) = buildTypeScope (VarScope NullScope []) stmts

buildTypeScope :: VarScope -> [Stmt] -> VarScope
buildTypeScope scope [] = scope
buildTypeScope scope (x:y) = buildTypeScope (processElemToScope x scope) y

buildTypeScopeThr :: VarScope -> [Stmt] -> VarScope
buildTypeScopeThr scope [] = scope
buildTypeScopeThr scope (x:y) = buildTypeScope (processElemToScopeThr x scope) y

-- ==========================================================================================================
-- Function to build to new type scope data structure from the current one for all statements of the program
-- ==========================================================================================================

processElemToScope :: Stmt -> VarScope -> VarScope
processElemToScope stmt scope =
  case stmt of (VarDecStmt varDec) -> processVarDec varDec scope
               (VarDecSpecialStmt varDecSpecial) -> processVarDecSpecial varDecSpecial scope
               (VarReDecStmt varReDec) -> processVarReDec varReDec scope
               (WheStmt whereStmt) -> processWhere whereStmt scope
               (IfStmt ifStmt) -> processIf ifStmt scope
               (ThreadStmt thrStmt) -> processThread thrStmt scope
               (LockStmt lockStmt) -> processLock lockStmt scope
               (PrintStmt varName) -> processPrint varName scope
               ExitStmt -> scope

processElemToScopeThr :: Stmt -> VarScope -> VarScope
processElemToScopeThr stmt scope =
  case stmt of (VarDecStmt varDec) -> processVarDec varDec scope
               (VarReDecStmt varReDec) -> processVarReDecThr varReDec scope
               (WheStmt whereStmt) -> processWhereThr whereStmt scope
               (IfStmt ifStmt) -> processIfThr ifStmt scope
               (ThreadStmt thrStmt) -> processThread thrStmt scope
               (LockStmt lockStmt) -> processLock lockStmt scope
               (PrintStmt varName) -> processPrint varName scope
               ExitStmt -> scope

-- ==========================================================================================================
-- Function to build to new type scope data structure from the current one for all statements of the program
-- ==========================================================================================================

processVarDec :: VarDec -> VarScope -> VarScope
processVarDec (VarDec elemType elemName elemVal) (VarScope motherScope elems)
  | existedInScope elemName elems = error (elemName ++ " already existed!!")
  | (getTypeV elemVal (VarScope motherScope elems)) /= elemType = error (elemName ++ " has invalid type: " ++ (show elemType))
  | otherwise = addToScope (ElemVar elemName elemType) (VarScope motherScope elems)

processVarDecSpecial :: VarDecSpecial -> VarScope -> VarScope
processVarDecSpecial (VarDecSpecial elemType elemName val) (VarScope motherScope elems)
  | existedInScope elemName elems = error (elemName  ++ " already existed!!")
  | existedAsGlobal elemName motherScope = error (elemName ++ " already existed!!")
  | (getTypeV val (VarScope motherScope elems)) /= elemType = error (elemName ++ " has invalid type: " ++ (show elemType))
  | otherwise = addToScope (ElemSharedVar elemName elemType) (VarScope motherScope elems)

processVarReDec :: VarReDec -> VarScope -> VarScope
processVarReDec (VarReDec varName newVal) scope = if (getType (findElem varName scope)) == (getTypeV newVal scope)
  then scope else error ("Invalid variable type redeclaring: " ++ varName)

processWhere :: Where -> VarScope -> VarScope
processWhere (Where condition s) scope = validCond condition scope (scpa scope s)

processIf :: If -> VarScope -> VarScope
processIf ifStmt scope =
  case ifStmt of (IfOne expr s1 s2) -> validCond expr scope (scpa (scpa scope s1) s2)
                 (IfTwo expr s) -> validCond expr scope (scpa scope s)

processThread :: Thread -> VarScope -> VarScope
processThread thread scope =
  case thread of (ThrStart thrName thrScope) -> processThrStart thrName thrScope scope
                 (ThrStop thrName) -> processVarCreated thrName Thr scope

processThrStart :: String -> Scope -> VarScope -> VarScope
processThrStart thrName scope varScope = scopeU2
  where scopeU1 = processNewVar thrName Thr varScope
        scopeU2 = scpaThr scopeU1 scope

processLock :: Lock -> VarScope -> VarScope
processLock lock scope =
  case lock of (LckCreate lockName) -> processNewVar lockName Lck scope
               (LckLock lockName) -> processVarCreated lockName Lck scope
               (LckUnlock lockName) -> processVarCreated lockName Lck scope

processPrint :: String -> VarScope -> VarScope
processPrint varName scope = (\_ -> scope) (findElem varName scope)

-- ==========================================================================================================
-- Function to build to new type scope data structure from the current one for all statements of the thread scope
-- ==========================================================================================================

processVarReDecThr :: VarReDec -> VarScope -> VarScope
processVarReDecThr (VarReDec varName newVal) scope = if (getType (findElemThr varName scope)) == (getTypeV newVal scope)
  then scope else error ("Invalid variable type redeclaring: " ++ varName)

processWhereThr :: Where -> VarScope -> VarScope
processWhereThr (Where condition s) scope = validCondThr condition scope (scpaThr scope s)

processIfThr :: If -> VarScope -> VarScope
processIfThr ifStmt scope =
  case ifStmt of (IfOne expr s1 s2) -> validCondThr expr scope (scpaThr (scpaThr scope s1) s2)
                 (IfTwo expr s) -> validCondThr expr scope (scpaThr scope s)

-- ==========================================================================================================
-- Helper functions to build the type scope data structure
-- ==========================================================================================================

processNewVar :: String -> VarType -> VarScope -> VarScope
processNewVar varName varType (VarScope motherScope elems)
  | existedInScope varName elems = error ("Variable " ++ varName ++ " already existed!!")
  | otherwise = addToScope (ElemVar varName varType) (VarScope motherScope elems)

processVarCreated :: String -> VarType -> VarScope -> VarScope
processVarCreated name varType scope
  | (\x -> (getType x) /= varType) (findElem name scope) = error (name ++ " is not " ++ (show varType))
  | otherwise = scope

addToScope :: ElemVar -> VarScope -> VarScope
addToScope x NullScope = VarScope NullScope [x]
addToScope x (VarScope y z) = VarScope y (z ++ [x])

existedInScope :: String -> [ElemVar] -> Bool
existedInScope _ [] = False
existedInScope varName (x:y) = case x of (ElemScope _) -> existedNext
                                         (ElemVar name _) -> if (name == varName) then True else existedNext
                                         (ElemSharedVar name _) -> if (name == varName) then True else existedNext
                               where existedNext = existedInScope varName y

existedAsGlobal :: String -> VarScope -> Bool
existedAsGlobal _ NullScope = False
existedAsGlobal var (VarScope mother []) = existedAsGlobal var mother
existedAsGlobal varName (VarScope mother (x:y)) = case x of (ElemSharedVar name _) -> if (name == varName) then True else existedNext
                                                            otherwise -> existedNext
                                                  where existedNext = existedAsGlobal varName (VarScope mother y)

validCond :: Expr -> VarScope -> a -> a
validCond cond scope x | (getTypeV cond scope) /= Boo = error ("Invalid type (Require BOO): " ++ (show cond))
                       | otherwise = x

scpa :: VarScope -> Scope -> VarScope
scpa scope (Scope stmts) = addToScope (ElemScope (buildTypeScope (VarScope scope []) stmts)) scope


getType :: ElemVar -> VarType
getType (ElemVar _ varType) = varType
getType (ElemSharedVar _ varType) = varType

getTypeV :: Expr -> VarScope -> VarType
getTypeV x scope = case x of (NumExp _) -> Num
                             (VarExp varName) -> getType (findElem varName scope)
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
              (ElemSharedVar name varType) -> if (elem == name) then (ElemSharedVar name varType) else findNext
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

-- ==========================================================================================================
-- Helper functions to build the type scope data structure
-- These functions are different than the section above because inside a thread scope, variables outside the scope cannot
-- be used unless they're global
-- ==========================================================================================================

findElemThr :: String -> VarScope -> ElemVar
findElemThr elem NullScope = error ("Cannot find elem " ++ elem)
findElemThr elem (VarScope NullScope []) = error ("Cannot find elem " ++ elem)
findElemThr elem (VarScope motherScope []) = findElemThr' elem motherScope
findElemThr elem (VarScope motherScope (x:y)) =
     case x of (ElemVar name varType) -> if (elem == name) then (ElemVar name varType) else findNext
               (ElemSharedVar name varType) -> if (elem == name) then (ElemSharedVar name varType) else findNext
               (ElemScope _) -> findNext
     where findNext = findElem elem (VarScope motherScope y)
findElemThr' :: String -> VarScope -> ElemVar
findElemThr' elem NullScope = error ("Cannot find elem " ++ elem)
findElemThr' elem (VarScope NullScope []) = error ("Cannot find elem " ++ elem)
findElemThr' elem (VarScope motherScope []) = findElemThr' elem motherScope
findElemThr' elem (VarScope motherScope (x:y)) =
     case x of (ElemSharedVar name varType) -> if (elem == name) then (ElemSharedVar name varType) else findNext
               otherwise -> findNext
     where findNext = findElem elem (VarScope motherScope y)

getTypeVThr :: Expr -> VarScope -> VarType
getTypeVThr x scope = case x of (NumExp _) -> Num
                                (VarExp varName) -> getType (findElemThr varName scope)
                                (BooExp _) -> Boo
                                (NumCalc ex1 _ ex2) -> isNum ex1 (isNum ex2 Num)
                                (BooCalc ex1 _ ex2) -> isBoo ex1 (isBoo ex2 Boo)
                                (CondExp ex1 _ ex2) -> isNum ex1 (isNum ex2 Boo)
                      where isNum x = valid (getTypeVThr x scope) Num
                            isBoo x = valid (getTypeVThr x scope) Boo
                            valid type1 type2 val = if (type1 == type2) then val else error ("Incorrect type" ++ (show x))

validCondThr :: Expr -> VarScope -> a -> a
validCondThr cond scope x | (getTypeVThr cond scope) /= Boo = error ("Invalid type (Require BOO): " ++ (show cond))
                          | otherwise = x

scpaThr :: VarScope -> Scope -> VarScope
scpaThr scope (Scope stmts) = addToScope (ElemScope (buildTypeScopeThr (VarScope scope []) stmts)) scope

-- ==========================================================================================================
-- Other helper functions
-- ==========================================================================================================

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