module CodeGen where

import Sprockell
import LangDef
import Debug.Trace
import Data.List

-- ==========================================================================================================
-- CodeGen data class/data structure to store information about the program and used to help generating instructions
-- ==========================================================================================================

type LocalAddr = Int -- from 8 to beyond
type SharedAddr = Int -- from 4 to 7
type ThrAddr = Int -- from 1 to 3 (0 is the default shared memory for main thread)

data VarMap = VarMap VarMap LocalAddr SharedAddr [VarMem]
            | NullMap
            deriving (Eq, Show)
data VarMem = LocalMem String AddrImmDI
            | SharedMem String AddrImmDI
            | ThrMem String AddrImmDI
            | MapMem VarMap
            deriving (Eq, Show)

notCreatedAddr = -1
startLA = 0
startSA = 4
startTA = 1

mapNextLA (VarMap a b c d) = VarMap a next c d
  where next = if b == notCreatedAddr then startLA else b + 1
mapNextLA NullMap = VarMap NullMap startLA notCreatedAddr []

mapNextSA (VarMap a b c d) = VarMap a b next d
  where next = if c == notCreatedAddr then startSA else c + 1
mapNextSA NullMap = VarMap NullMap notCreatedAddr startSA []

nextLA (VarMap _ a _ _) = DirAddr (if a == notCreatedAddr then startLA else a + 1)
nextLA NullMap = DirAddr startLA
nextSA (VarMap _ _ a _) = DirAddr (if a == notCreatedAddr then startSA else a + 1)
nextSA NullMap = DirAddr startSA

getLA (VarMap _ a _ _) = DirAddr a
getLA NullMap = DirAddr startLA
getSA (VarMap _ _ a _) = DirAddr a
getSA (NullMap) = DirAddr startSA

lAddr (VarMap _ a _ _) = a
sAddr (VarMap _ _ a _) = a

-- ==========================================================================================================
-- Main function used to generate instructions for a program
-- ==========================================================================================================

genCode :: Program -> [[Instruction]]
genCode prog = trace (show instrs) (genRP instrs (countThrNum prog + 1) [])
  where (instrs, map) = genInstrs prog
        genRP _ 0 x = x
        genRP l num x = genRP l (num - 1) (l : x)

-- ==========================================================================================================
-- Function used to generate instructions
-- ==========================================================================================================

genInstrs :: Program -> ([Instruction], VarMap)
genInstrs (Program stmts) = (intrs ++ [EndProg], map)
  where (intrs, map) = genInstrs' NullMap stmts []

genInstrs' :: VarMap -> [Stmt] -> [Instruction] -> ([Instruction], VarMap)
genInstrs' map [] instrs = (instrs, map)
genInstrs' map (x:y) instrs = genInstrs' newMap y (instrs ++ instr)
  where (instr, newMap) = genStmt x map

-- ==========================================================================================================
-- Functions to generate instructions from statements
-- ==========================================================================================================

genStmt :: Stmt -> VarMap -> ([Instruction], VarMap)
genStmt stmt map = case stmt of (VarDecStmt (VarDec _ varName val)) -> genVarDec map varName val
                                (VarDecSpecialStmt (VarDecSpecial _ varName val)) -> genVarDecSpecial map varName val
                                (VarReDecStmt (VarReDec varName val)) -> genVarReDec map varName val
                                (WheStmt (Where expr scope)) -> genWhere map expr scope
                                (IfStmt ifStmt) -> genIf map ifStmt
                                (LockStmt lockStmt) -> genLock map lockStmt
                                (ThreadStmt thrStmt) -> genThread map thrStmt
                                (PrintStmt varName) -> genVarPrint map varName
                                ExitStmt -> (genExit, map)

-- ==========================================================================================================
-- Functions to generate instructions from statement details
-- ==========================================================================================================

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec map varName expr = (instrs ++ [ Load (getLA map1) regA, Store regA (getLA map2)], map3)
  where (instrs, map1) = genExpr map expr
        map2 = mapNextLA map
        map3 = storeVar (LocalMem varName (getLA map2)) map2

genVarDecSpecial :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDecSpecial map varName expr = (instrs ++ [ Load (getLA map1) regA, WriteInstr regA (getSA map2)], map3)
  where (instrs, map1) = genExpr map expr
        map2 = mapNextSA map
        map3 = storeVar (SharedMem varName (getSA map2)) map2

genVarReDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarReDec map varName expr = (instrs ++ varReDecInstrs, map)
  where varReDecInstrs = if shared then sharedInstrs else localInstrs
        sharedInstrs = [ Load (getLA map1) regA, WriteInstr regA mem ]
        localInstrs = [ Load (getLA map1) regA, Store regA mem ]
        (mem, shared) = findVarMem varName map
        (instrs, map1) = genExpr map expr


genWhere :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genWhere map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA,
                                      Load (getLA map1) regB,
                                      Compute Sub regA regB regB,
                                      Branch regB (Rel (length scopeInstrs + 2)) ] ++
                                    scopeInstrs ++
                                    [ Jump (Rel (- (length scopeInstrs) - 4 - (length exprInstrs))) ]
                                    , map3)
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs, map2) = genInstrs' (VarMap map (lAddr map) (sAddr map) []) stmts []
        map3 = storeVar (MapMem map2) map

genIf :: VarMap -> If -> ([Instruction], VarMap)
genIf map ifStmt = case ifStmt of (IfOne expr scope1 scope2) -> genIfOne map expr scope1 scope2
                                  (IfTwo expr scope) -> genIfTwo map expr scope

genIfOne :: VarMap -> Expr -> Scope -> Scope -> ([Instruction], VarMap)
genIfOne map expr (Scope stmts1) (Scope stmts2) = ( exprInstrs ++
                                                    [ Load (ImmValue 1) regA,
                                                      Load (getLA map1) regB,
                                                      Compute Sub regA regB regB,
                                                      Branch regB (Rel (length scopeInstrs1 + 2)) ] ++
                                                    scopeInstrs1 ++
                                                    [ Jump (Rel (length scopeInstrs2 + 1)) ] ++
                                                    scopeInstrs2
                                                  , map5 )
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs1, map2) = genInstrs' (VarMap map (lAddr map) (sAddr map) []) stmts1 []
        map3 = storeVar (MapMem map2) map
        (scopeInstrs2, map4) = genInstrs' (VarMap map3 (lAddr map3) (sAddr map3) []) stmts2 []
        map5 = storeVar (MapMem map4) map3


genIfTwo :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genIfTwo map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA
                                    , Load (getLA map1) regB
                                    , Compute Sub regA regB regB
                                    , Branch regB (Rel (length scopeInstrs + 1))] ++
                                    scopeInstrs
                                  , map3)
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs, map2) = genInstrs' (VarMap map1 (lAddr map) (sAddr map) []) stmts []
        map3 = storeVar (MapMem map2) map1


genThread :: VarMap -> Thread -> ([Instruction], VarMap)
genThread map thr = case thr of (ThrStart thrName scope) -> genThrStart map thrName scope
                                (ThrStop thrName) -> genThrStop map thrName

genThrStart :: VarMap -> String -> Scope -> ([Instruction], VarMap)
genThrStart map thrName scope = ([], map)

genThrStop :: VarMap -> String -> ([Instruction], VarMap)
genThrStop map thrName = ([], map)

genLock :: VarMap -> Lock -> ([Instruction], VarMap)
genLock varMap lock = case lock of (LckCreate lockName) -> genLockCreate varMap lockName
                                   (LckLock lockName) -> genLockLock varMap lockName
                                   (LckUnlock lockName) -> genLockUnlock varMap lockName

genLockCreate :: VarMap -> String -> ([Instruction], VarMap)
genLockCreate map lockName = ([Load (ImmValue 0) regA, WriteInstr regA addr], map1)
  where map1 = storeVar (SharedMem lockName addr) map
        addr = nextSA map

genLockLock :: VarMap -> String -> ([Instruction], VarMap)
genLockLock map lockName = (instrs, map)
  where instrs = [ TestAndSet (fst $ findVarMem lockName map)
                 , Receive regA
                 , Load (ImmValue 1) regB
                 , Compute Sub regB regA regA
                 , Branch regA (Rel (-4))
                 ]

genLockUnlock :: VarMap -> String -> ([Instruction], VarMap)
genLockUnlock varMap lockName = (instrs, varMap)
  where instrs = [ Load (ImmValue 0) regA
                 , WriteInstr regA (fst $ findVarMem lockName varMap)]

genVarPrint :: VarMap -> String -> ([Instruction], VarMap)
genVarPrint map varName = (if shared then sharedInstrs else localInstrs, map)
  where localInstrs = [ Load mem regA , WriteInstr regA numberIO ]
        sharedInstrs = [ ReadInstr mem , Receive regA, WriteInstr regA numberIO ]
        (mem, shared) = findVarMem varName map

genExit :: [Instruction]
genExit = [ EndProg ]

-- ==========================================================================================================
-- Functions to generate instructions for expressions
-- ==========================================================================================================

genExpr :: VarMap -> Expr -> ([Instruction], VarMap)
genExpr map expr = case expr of (NumExp val) -> genNum map val
                                (BooExp val) -> genBool map val
                                (VarExp val) -> genVar map val
                                (NumCalc ex1 op ex2) -> genNumCalc map ex1 op ex2
                                (BooCalc ex1 op ex2) -> genCalc map ex1 (mapBoolOp op) ex2
                                (CondExp ex1 op ex2) -> genCalc map ex1 (mapOrdOp op) ex2
                                (Parens ex) -> genExpr map ex

genNum :: VarMap -> Integer -> ([Instruction], VarMap)
genNum map num = ([ Load (ImmValue $ fromInteger $ num) regA
                  , Store regA (nextLA map)], mapNextLA map)

genBool :: VarMap -> Bool -> ([Instruction], VarMap)
genBool map bool = ([ Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA
                    , Store regA (nextLA map)], mapNextLA map)

genVar :: VarMap -> String -> ([Instruction], VarMap)
genVar map var =  ( if shared then sharedInstrs else localInstrs, mapNextLA map)
  where localInstrs = [ Load mem regA, Store regA (nextLA map) ]
        sharedInstrs = [ ReadInstr mem, Receive regA, Store regA (nextLA map) ]
        (mem, shared) = findVarMem var map

genNumCalc :: VarMap -> Expr -> CalcOp -> Expr -> ([Instruction], VarMap)
genNumCalc map ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = genCalc map ex1 (mapCalcOp op) ex2
    | otherwise =  (instrs1 ++
                   instrs2 ++
                   [ Load (getLA map1) regA
                   , Load (getLA map2) regB
                   , Load (ImmValue 0) regC
                   , Compute Lt regA regB regD
                   , Branch regD (Rel 4)
                   , Compute Sub regA regB regA
                   , Compute Incr regC regC regC
                   , Jump (Rel (-4))
                   , Store regC (nextLA map)
                   ], mapNextLA map)
                   where (instrs1, map1) = (genExpr map ex1)
                         (instrs2, map2) = (genExpr map1 ex2)

genCalc :: VarMap-> Expr -> Operator -> Expr -> ([Instruction], VarMap)
genCalc map ex1 op ex2 = ( instrs1 ++
                           instrs2 ++
                          [ Load (getLA map1) regA
                          , Load (getLA map2) regB
                          , Compute op regA regB regC
                          , Store regC (nextLA map)
                          ], mapNextLA map)
                          where (instrs1, map1) = (genExpr map ex1)
                                (instrs2, map2) = (genExpr map1 ex2)

-- ==========================================================================================================
-- Helpers functions to generate instructions
-- ==========================================================================================================

findVarMem :: String -> VarMap -> (AddrImmDI, Bool)
findVarMem varNem NullMap = error ("Cannot find " ++ varNem)
findVarMem varMem (VarMap motherMap _ _ []) = findVarMem varMem motherMap
findVarMem varMem (VarMap motherMap a s (x:y)) = case x of (LocalMem memName memAddr) -> if (varMem == memName) then (memAddr, False) else findNext
                                                           (SharedMem memName memAddr) -> if (varMem == memName) then (memAddr, True) else findNext
                                                           otherwise -> findNext
                                               where findNext = findVarMem varMem (VarMap motherMap a s y)

storeVar :: VarMem -> VarMap -> VarMap
storeVar (LocalMem str addr) NullMap = VarMap NullMap (dirToAddr addr) notCreatedAddr [LocalMem str addr]
storeVar (LocalMem str addr) (VarMap m a s l) = VarMap m a s (l ++ [LocalMem str addr])
storeVar (SharedMem str addr) NullMap = VarMap NullMap notCreatedAddr (dirToAddr addr) [SharedMem str addr]
storeVar (SharedMem str addr) (VarMap m a s l) = VarMap m a s (l ++ [SharedMem str addr])
storeVar (MapMem (VarMap mother a s l)) NullMap = VarMap NullMap a s [MapMem (VarMap mother a s l)]
storeVar (MapMem (VarMap m1 a1 s1 l1)) (VarMap m2 a2 s2 l2) = VarMap m2 a1 s1 (l2 ++ [MapMem (VarMap m1 a1 s1 l1)])

dirToAddr addr = case addr of (DirAddr x) -> x
                              otherwise -> error "Invalid addr"

countThrNum :: Program -> Integer
countThrNum (Program stmts) = countThrNum' stmts 0

countThrNum' :: [Stmt] -> Integer -> Integer
countThrNum' [] x = x
countThrNum' (x:y) n = case x of (ThreadStmt (ThrStart _ _)) -> countThrNum' y (n+1)
                                 otherwise -> countThrNum' y n

-- ==========================================================================================================
-- Mapping of language operators into instruction operators
-- ==========================================================================================================

mapOrdOp :: OrdOp -> Operator
mapOrdOp L = Lt
mapOrdOp M = Gt
mapOrdOp E = Equal
mapOrdOp LE = LtE
mapOrdOp ME = GtE
mapOrdOp NE = NEq

mapCalcOp :: CalcOp -> Operator
mapCalcOp AddOp = Add
mapCalcOp SubOp = Sub
mapCalcOp Mult = Mul

mapBoolOp :: BoolOp -> Operator
mapBoolOp AndOp = And
mapBoolOp OrOp = Or