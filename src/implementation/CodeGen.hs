module CodeGen where

import Sprockell
import LangDef
import Debug.Trace
import Data.List

type ThrAddr = AddrImmDI
type CurrentAddr = Int

data VarMap = VarMap VarMap CurrentAddr [VarMem]
            | NullMap
            deriving (Eq, Show)
data VarMem = VarMem String AddrImmDI
            | MapMem VarMap
            | ThrMem String AddrImmDI VarMap [Instruction] ThrAddr
            deriving (Eq, Show)

mapNextAddr (VarMap a b c) = VarMap a (b+1) c
mapNextAddr NullMap = VarMap NullMap startAddr []
nextAddr (VarMap _ a _) = DirAddr (a + 1)
nextAddr NullMap = DirAddr startAddr
getAddr (VarMap _ a _) = DirAddr a
getAddr NullMap = DirAddr startAddr
getAddrN (VarMap _ a _) = a
startAddr = 0

maxThread = 4
thr1 = DirAddr 0
thr2 = DirAddr 1
thr3 = DirAddr 2
thr4 = DirAddr 3

--nextThrAddr map scopeMap = DirAddr $ (countThr map) + (countThrD scopeMap) + 1

findVarMem :: String -> VarMap -> AddrImmDI
findVarMem varNem NullMap = error ("Cannot find " ++ varNem)
findVarMem varMem (VarMap motherMap _ []) = findVarMem varMem motherMap
findVarMem varMem (VarMap motherMap a (x:y)) = case x of (VarMem memName memAddr) -> if (varMem == memName) then memAddr else findNext
                                                         otherwise -> findNext
                                             where findNext = findVarMem varMem (VarMap motherMap a y)

--findThrMem :: String -> VarMap -> VarMem
--findThrMem thrName NullMap = error ("Cannot find " ++ thrName)
--findThrMem thrName (VarMap motherMap []) = findThrMem thrName motherMap
--findThrMem thrName (VarMap motherMap (x:y)) = case x of (ThrMem name a b c d) -> if (thrName == name) then (ThrMem name a b c d) else findNext
--                                                        otherwise -> findNext
--                                             where findNext = findThrMem thrName (VarMap motherMap y)

storeVar :: VarMem -> VarMap -> VarMap
storeVar (VarMem str addr) NullMap = VarMap NullMap ((\(DirAddr x) -> x) addr) [VarMem str addr]
storeVar (MapMem (VarMap mother addr l)) NullMap = VarMap NullMap addr [MapMem (VarMap mother addr l)]
storeVar (VarMem str addr) (VarMap m curAddr l) = VarMap m curAddr (l ++ [VarMem str addr])
storeVar (MapMem (VarMap m1 a1 l1)) (VarMap m2 a2 l2) = VarMap m2 (a1 + a2) (l2 ++ [MapMem (VarMap m1 a1 l1)])
--countThr :: VarMap -> Int
--countThr map = case map of NullMap -> 0
--                           (VarMap mother mem) -> (countThrD map) + (countThrU mother map)
--
--countThrU' :: VarMap -> [VarMem] -> Int
--countThrU' _ [] = 0
--countThrU' mapToAvoid (x:y) = case x of (VarMem _ _) -> next
--                                        (MapMem map) -> if (map == mapToAvoid) then next else (countThrD map) + next
--                                        (ThrMem _ _ map _ _) -> if (map == mapToAvoid) then 1 + next else (countThrD map) + next + 1
--                              where next = countThrU' mapToAvoid y
countThrD :: VarMap -> Int
countThrD NullMap = 0
countThrD (VarMap _ _ mem) = countThrD' mem

countThrD' :: [VarMem] -> Int
countThrD' [] = 0
countThrD' (x:y) = case x of (VarMem _ _) -> countThrD' y
                             (MapMem map) -> countThrD map
                             (ThrMem _ _ map _ _) -> 1 + countThrD map
--
--countThrU :: VarMap -> VarMap -> Int
--countThrU map child = case map of NullMap -> 0
--                                  (VarMap mother mem) -> (countThrU mother map) + (countThrU' child mem)

genCode :: Program -> [[Instruction]]
genCode prog = genRP updatedInstrs (countThrD map + 1) []
  where (instrs, map) = genInstrs prog
        updatedInstrs = updateNumInstrs instrs
        genRP _ 0 x = x
        genRP l num x = genRP l (num - 1) (l : x)

genInstrs :: Program -> ([Instruction], VarMap)
genInstrs (Program stmts) = (intrs ++ [EndProg], map)
  where (intrs, map) = genInstrs' NullMap stmts []

genInstrs' :: VarMap -> [Stmt] -> [Instruction] -> ([Instruction], VarMap)
genInstrs' map [] instrs = (instrs, map)
genInstrs' map (x:y) instrs = genInstrs' newMap y (instrs ++ instr)
  where (instr, newMap) = genStmt x map

genStmt :: Stmt -> VarMap -> ([Instruction], VarMap)
genStmt stmt map = case stmt of (VarDecStmt (VarDec _ varName val)) -> genVarDec map varName val
                                (VarReDecStmt (VarReDec varName val)) -> genVarReDec map varName val
                                (WheStmt (Where expr scope)) -> genWhere map expr scope
                                (IfStmt ifStmt) -> genIf map ifStmt
--                                (LockStmt lockStmt) -> genLock map lockStmt
--                                (ThreadStmt thrStmt) -> genThread map thrStmt
                                (PrintStmt varName) -> genVarPrint map varName
                                ExitStmt -> (genExit, map)
genExit :: [Instruction]
genExit = [ EndProg ]

--genThread :: VarMap -> Thread -> ([Instruction], VarMap)
--genThread map thr = case thr of (ThrCreate thrName scope) -> genThrCreate map thrName scope
--                                (ThrStart thrName) -> genThrStart map thrName
--                                (ThrStop thrName) -> genThrStop map thrName

--genThrCreate :: VarMap -> String -> Scope -> ([Instruction], VarMap)
--genThrCreate map thrName (Scope stmts) = ([], updatedMap)
--  where updatedMap = storeVar thrMem map
--        thrMem = ThrMem thrName (nextThrAddr map scopeMap) scopeMap scopeInstrs (nextAddr map) -- TODO: Check if there are any problems
--        (scopeInstrs, scopeMap) = genInstrs' (VarMap map []) stmts []
--
--genThrStart :: VarMap -> String -> ([Instruction], VarMap)
--genThrStart map thrName = genThrStart' map (findThrMem thrName map)
--
--genThrStart' :: VarMap -> VarMem -> ([Instruction], VarMap)
--genThrStart' map (ThrMem _ addr _ instrs thrAddr) = ([ ] , map) -- TODO
--
--genThrStop :: VarMap -> String -> ([Instruction], VarMap)
--genThrStop map thrName = ([], map) -- TODO
--  where (ThrMem _ _ map _ _) = findThrMem thrName map

genVarPrint :: VarMap -> String -> ([Instruction], VarMap)
genVarPrint map varName = ([ Load (findVarMem varName map) regA , WriteInstr regA numberIO ], map)

--genLock :: VarMap -> Lock -> ([Instruction], VarMap)
--genLock varMap lock = case lock of (LckCreate lockName) -> genLockCreate varMap lockName
--                                   (LckLock lockName) -> genLockLock varMap lockName
--                                   (LckUnlock lockName) -> genLockUnlock varMap lockName
--
--genLockCreate :: VarMap -> String -> ([Instruction], VarMap)
--genLockCreate varMap lockName = (instrs, newVarMap)
--  where instrs = [Load (ImmValue 0) regA
--                 , Store regA addr]
--        newVarMap = storeVar (VarMem lockName addr) varMap
--        addr = nextAddr varMap
--
--genLockLock :: VarMap -> String -> ([Instruction], VarMap)
--genLockLock varMap lockName = (instrs, varMap)
--  where instrs = [ TestAndSet (findVarMem lockName varMap)
--                 , Receive regA
--                 , Load (ImmValue 1) regB
--                 , Compute Sub regB regA regA
--                 , Branch regA (Rel (-4))
--                 ]
--
--genLockUnlock :: VarMap -> String -> ([Instruction], VarMap)
--genLockUnlock varMap lockName = (instrs, varMap)
--  where instrs = [ Load (ImmValue 0) regA
--                 , WriteInstr regA (findVarMem lockName varMap)]

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec map varName expr = (instrs, map2)
  where (instrs, map1) = genExpr map expr
        map2 = storeVar (VarMem varName (getAddr map1)) map1

genVarReDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarReDec map varName expr = (instrs ++
                                [ Load (getAddr map1) regA
                                , Store regA varMem ], map1)
  where varMem = findVarMem varName map
        (instrs, map1) = genExpr map expr

genWhere :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genWhere map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA,
                                      Load (getAddr map1) regB,
                                      Compute Sub regA regB regB,
                                      Branch regB (Rel (length scopeInstrs + 2)) ] ++
                                    scopeInstrs ++
                                    [ Jump (Rel (- (length scopeInstrs) - 4 - (length exprInstrs))) ]
                                    , map3)
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs, map2) = genInstrs' (VarMap map1 (getAddrN map1) []) stmts []
        map3 = storeVar (MapMem map2) map1


genIf :: VarMap -> If -> ([Instruction], VarMap)
genIf map ifStmt = case ifStmt of (IfOne expr scope1 scope2) -> genIfOne map expr scope1 scope2
                                  (IfTwo expr scope) -> genIfTwo map expr scope
--
genIfOne :: VarMap -> Expr -> Scope -> Scope -> ([Instruction], VarMap)
genIfOne map expr (Scope stmts1) (Scope stmts2) = ( exprInstrs ++
                                                    [ Load (ImmValue 1) regA,
                                                      Load (getAddr map1) regB,
                                                      Compute Sub regA regB regB,
                                                      Branch regB (Rel (length scopeInstrs1 + 2)) ] ++
                                                    scopeInstrs1 ++
                                                    [ Jump (Rel (length scopeInstrs2 + 1)) ] ++
                                                    scopeInstrs2
                                                  , map5 )
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs1, map2) = genInstrs' (VarMap map1 (getAddrN map1) []) stmts1 []
        map3 = storeVar (MapMem map2) map1
        (scopeInstrs2, map4) = genInstrs' (VarMap map3 (getAddrN map3) []) stmts2 []
        map5 = storeVar (MapMem map4) map3


genIfTwo :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genIfTwo map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA
                                    , Load (getAddr map1) regB
                                    , Compute Sub regA regB regB
                                    , Branch regB (Rel (length scopeInstrs + 1))] ++
                                    scopeInstrs
                                  , map3)
  where (exprInstrs, map1) = genExpr map expr
        (scopeInstrs, map2) = genInstrs' (VarMap map1 (getAddrN map1) []) stmts []
        map3 = storeVar (MapMem map2) map1


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
                  , Store regA (nextAddr map)], mapNextAddr map)

genBool :: VarMap -> Bool -> ([Instruction], VarMap)
genBool map bool = ([ Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA
                    , Store regA (nextAddr map)], mapNextAddr map)

genVar :: VarMap -> String -> ([Instruction], VarMap)
genVar map var =  ([ Load (findVarMem var map) regA
                       , Store regA (nextAddr map)], mapNextAddr map)

genNumCalc :: VarMap -> Expr -> CalcOp -> Expr -> ([Instruction], VarMap)
genNumCalc map ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = genCalc map ex1 (mapCalcOp op) ex2
    | otherwise =  (instrs1 ++
                   instrs2 ++
                   [ Load (getAddr map1) regA
                   , Load (getAddr map2) regB
                   , Load (ImmValue 0) regC
                   , Compute Lt regA regB regD
                   , Branch regD (Rel 4)
                   , Compute Sub regA regB regA
                   , Compute Incr regC regC regC
                   , Jump (Rel (-4))
                   , Store regC (nextAddr map2)
                   ], mapNextAddr map2)
                   where (instrs1, map1) = (genExpr map ex1)
                         (instrs2, map2) = (genExpr map1 ex2)
--genExpr :: VarMap -> Expr -> ([Instruction], VarMap)

genCalc :: VarMap-> Expr -> Operator -> Expr -> ([Instruction], VarMap)
genCalc map ex1 op ex2 = ( instrs1 ++
                           instrs2 ++
                          [ Load (getAddr map1) regA
                          , Load (getAddr map2) regB
                          , Compute op regA regB regC
                          , Store regC (nextAddr map2)
                          ], mapNextAddr map2)
                          where (instrs1, map1) = (genExpr map ex1)
                                (instrs2, map2) = (genExpr map1 ex2)

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

updateNumInstrs :: [Instruction] -> [Instruction]
updateNumInstrs instrs = updateNumInstrs' (zip (usedAddr instrs) [0..]) instrs
updateNumInstrs' :: [(Int, Int)] -> [Instruction] -> [Instruction]
updateNumInstrs' [] x = x
updateNumInstrs' (x:y) z = updateNumInstrs' y (map (upInsNum x) z)

usedAddr :: [Instruction] -> [Int]
usedAddr instrs = take (length res - 1) res
  where res = sort $ nub $ usedAddr' instrs []
usedAddr' :: [Instruction] -> [Int] -> [Int]
usedAddr' [] x = x
usedAddr' (ins:y) x = case ins of (Load a _) -> next a
                                  (Store _ a) -> next a
                                  (ReadInstr a) -> next a
                                  (WriteInstr _ a) -> next a
                                  (TestAndSet a) -> next a
                                  otherwise -> usedAddr' y x
  where next (DirAddr addr) = usedAddr' y (x ++ [addr])
        next _ = usedAddr' y x

upInsNum :: (Int,Int) -> Instruction -> Instruction
upInsNum (x,y) ins = case ins of (Load a b) -> (Load (f a) b)
                                 (Store a b) -> (Store a (f b))
                                 (ReadInstr a) -> ReadInstr (f a)
                                 (WriteInstr a b) -> WriteInstr a (f b)
                                 (TestAndSet a) -> TestAndSet (f a)
                                 otherwise -> ins
  where f (DirAddr addr) = if x == addr then DirAddr y else (DirAddr addr)
        f m = m