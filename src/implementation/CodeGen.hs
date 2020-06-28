module CodeGen where

import Sprockell
import LangDef
import Debug.Trace

type ThrAddr = AddrImmDI

data VarMap = VarMap VarMap [VarMem]
            | NullMap deriving (Eq, Show)
data VarMem = VarMem String AddrImmDI
            | MapMem VarMap
            | ThrMem String AddrImmDI VarMap [Instruction] ThrAddr deriving (Eq, Show)

tempAddresses = 2
temp1 = DirAddr 4
temp2 = DirAddr 5

maxThread = 4
thr1 = DirAddr 0
thr2 = DirAddr 1
thr3 = DirAddr 2
thr4 = DirAddr 3

nextAddr map =  DirAddr $ (countMap map) + maxThread + tempAddresses
nextThrAddr map scopeMap = DirAddr $ (countThr map) + (countThrD scopeMap) + 1


findVarMem :: String -> VarMap -> AddrImmDI
findVarMem varNem NullMap = error ("Cannot find " ++ varNem)
findVarMem varMem (VarMap motherMap []) = findVarMem varMem motherMap
findVarMem varMem (VarMap motherMap (x:y)) = case x of (VarMem memName memAddr) -> if (varMem == memName) then memAddr else findNext
                                                       otherwise -> findNext
                                             where findNext = findVarMem varMem (VarMap motherMap y)

findThrMem :: String -> VarMap -> VarMem
findThrMem thrName NullMap = error ("Cannot find " ++ thrName)
findThrMem thrName (VarMap motherMap []) = findThrMem thrName motherMap
findThrMem thrName (VarMap motherMap (x:y)) = case x of (ThrMem name a b c d) -> if (thrName == name) then (ThrMem name a b c d) else findNext
                                                        otherwise -> findNext
                                             where findNext = findThrMem thrName (VarMap motherMap y)

storeVar :: VarMem -> VarMap -> VarMap
storeVar var map = case map of NullMap -> VarMap NullMap [var]
                               (VarMap m mems) -> VarMap m (mems ++ [var])

countThr :: VarMap -> Int
countThr map = case map of NullMap -> 0
                           (VarMap mother mem) -> (countThrD map) + (countThrU mother map)

-----------------
countThrU' :: VarMap -> [VarMem] -> Int
countThrU' _ [] = 0
countThrU' mapToAvoid (x:y) = case x of (VarMem _ _) -> next
                                        (MapMem map) -> if (map == mapToAvoid) then next else (countThrD map) + next
                                        (ThrMem _ _ map _ _) -> if (map == mapToAvoid) then 1 + next else (countThrD map) + next + 1
                              where next = countThrU' mapToAvoid y
countThrD :: VarMap -> Int
countThrD NullMap = 0
countThrD (VarMap _ mem) = countThrD' mem

countThrD' :: [VarMem] -> Int
countThrD' [] = 0
countThrD' (x:y) = case x of (VarMem _ _) -> countThrD' y
                             (MapMem map) -> countThrD map
                             (ThrMem _ _ map _ _) -> 1 + countThrD map

countThrU :: VarMap -> VarMap -> Int
countThrU map child = case map of NullMap -> 0
                                  (VarMap mother mem) -> (countThrU mother map) + (countThrU' child mem)


-----------------
countMap :: VarMap -> Int
countMap map = case map of NullMap -> 0
                           (VarMap mother mem) -> (countMapD map) + (countMapU mother map)

countMapU :: VarMap -> VarMap -> Int
countMapU map child = case map of NullMap -> 0
                                  (VarMap mother mem) -> (countMapU mother map) + (countMapU' child mem)

countMapU' :: VarMap -> [VarMem] -> Int
countMapU' _ [] = 0
countMapU' mapToAvoid (x:y) = case x of (VarMem _ _) -> 1 + next
                                        (MapMem map) ->  if (map == mapToAvoid) then next else (countMapD map) + next
                                        (ThrMem _ _ map _ _) -> if (map == mapToAvoid) then 1 + next else (countMapD map) + next + 1
                              where next = countMapU' mapToAvoid y
countMapD :: VarMap -> Int
countMapD NullMap = 0
countMapD (VarMap _ mem) = countMapD' mem

countMapD' :: [VarMem] -> Int
countMapD' [] = 0
countMapD' (x:y) = case x of (VarMem _ _) -> 1 + (countMapD' y)
                             (MapMem map) -> countMapD map
                             (ThrMem _ _ map _ _) -> 1 + countMapD map

genCode :: Program -> [[Instruction]]
genCode prog = genRP instrs (countThrD map + 1) []
  where (instrs, map) = genInstrs prog
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
                                (VarReDecStmt (VarReDec varName val)) -> (genVarReDec map varName val, map)
                                (WheStmt (Where expr scope)) -> genWhere map expr scope
                                (IfStmt ifStmt) -> genIf map ifStmt
                                (LockStmt lockStmt) -> genLock map lockStmt
                                (ThreadStmt thrStmt) -> genThread map thrStmt
                                (PrintStmt varName) -> genVarPrint map varName
                                ExitStmt -> (genExit, map)
genExit :: [Instruction]
genExit = [ EndProg ]

genThread :: VarMap -> Thread -> ([Instruction], VarMap)
genThread map thr = case thr of (ThrCreate thrName scope) -> genThrCreate map thrName scope
                                (ThrStart thrName) -> genThrStart map thrName
                                (ThrStop thrName) -> genThrStop map thrName

genThrCreate :: VarMap -> String -> Scope -> ([Instruction], VarMap)
genThrCreate map thrName (Scope stmts) = ([], updatedMap)
  where updatedMap = storeVar thrMem map
        thrMem = ThrMem thrName (nextThrAddr map scopeMap) scopeMap scopeInstrs (nextAddr map) -- TODO: Check if there are any problems
        (scopeInstrs, scopeMap) = genInstrs' (VarMap map []) stmts []

genThrStart :: VarMap -> String -> ([Instruction], VarMap)
genThrStart map thrName = genThrStart' map (findThrMem thrName map)

genThrStart' :: VarMap -> VarMem -> ([Instruction], VarMap)
genThrStart' map (ThrMem _ addr _ instrs thrAddr) = ([ ] , map) -- TODO

genThrStop :: VarMap -> String -> ([Instruction], VarMap)
genThrStop map thrName = ([], map) -- TODO
  where (ThrMem _ _ map _ _) = findThrMem thrName map

genVarPrint :: VarMap -> String -> ([Instruction], VarMap)
genVarPrint map varName = ([ Load (findVarMem varName map) regA , WriteInstr regA numberIO ], map)

genLock :: VarMap -> Lock -> ([Instruction], VarMap)
genLock varMap lock = case lock of (LckCreate lockName) -> genLockCreate varMap lockName
                                   (LckLock lockName) -> genLockLock varMap lockName
                                   (LckUnlock lockName) -> genLockUnlock varMap lockName

genLockCreate :: VarMap -> String -> ([Instruction], VarMap)
genLockCreate varMap lockName = (instrs, newVarMap)
  where instrs = [Load (ImmValue 0) regA
                 , Store regA addr]
        newVarMap = storeVar (VarMem lockName addr) varMap
        addr = nextAddr varMap

genLockLock :: VarMap -> String -> ([Instruction], VarMap)
genLockLock varMap lockName = (instrs, varMap)
  where instrs = [ TestAndSet (findVarMem lockName varMap)
                 , Receive regA
                 , Load (ImmValue 1) regB
                 , Compute Sub regB regA regA
                 , Branch regA (Rel (-4))
                 ]

genLockUnlock :: VarMap -> String -> ([Instruction], VarMap)
genLockUnlock varMap lockName = (instrs, varMap)
  where instrs = [ Load (ImmValue 0) regA
                 , WriteInstr regA (findVarMem lockName varMap)]

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec varMap varName expr = (instrs, newVarMap)
  where instrs = genExpr varMap addr expr
        newVarMap = storeVar (VarMem varName addr) varMap
        addr = nextAddr varMap

genVarReDec :: VarMap -> String -> Expr -> [Instruction]
genVarReDec map varName expr = genExpr map (findVarMem varName map) expr

genWhere :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genWhere map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA,
                                      Load temp1 regB,
                                      Compute Sub regA regB regB,
                                      Branch regB (Rel (length scopeInstrs + 2)) ] ++
                                    scopeInstrs ++
                                    [ Jump (Rel (- (length scopeInstrs) - 4 - (length exprInstrs))) ]
                                    , updatedMap2)
  where exprInstrs = genExpr map temp1 expr
        updatedMap2 = storeVar (MapMem updatedMap) map
        (scopeInstrs, updatedMap) = genInstrs' (VarMap map []) stmts []

genIf :: VarMap -> If -> ([Instruction], VarMap)
genIf map ifStmt = case ifStmt of (IfOne expr scope1 scope2) -> genIfOne map expr scope1 scope2
                                  (IfTwo expr scope) -> genIfTwo map expr scope

genIfOne :: VarMap -> Expr -> Scope -> Scope -> ([Instruction], VarMap)
genIfOne map expr (Scope stmts1) (Scope stmts2) = ( exprInstrs ++
                                                    [ Load (ImmValue 1) regA,
                                                      Load temp1 regB,
                                                      Compute Sub regA regB regB,
                                                      Branch regB (Rel (length scopeInstrs1 + 2)) ] ++
                                                    scopeInstrs1 ++
                                                    [ Jump (Rel (length scopeInstrs2 + 1)) ] ++
                                                    scopeInstrs2
                                                  , updatedMap4 )
  where exprInstrs = genExpr map temp1 expr
        updatedMap4 = storeVar (MapMem updatedMap3) updatedMap2
        (scopeInstrs2, updatedMap3) = genInstrs' (VarMap updatedMap2 []) stmts2 []
        updatedMap2 = storeVar (MapMem updatedMap1) map
        (scopeInstrs1, updatedMap1) = genInstrs' (VarMap map []) stmts1 []

genIfTwo :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genIfTwo map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA
                                    , Load temp1 regB
                                    , Compute Sub regA regB regB
                                    , Branch regB (Rel (length scopeInstrs + 1))] ++
                                    scopeInstrs
                                  , updatedMap2)
  where exprInstrs = genExpr map temp1 expr
        updatedMap2 = storeVar (MapMem updatedMap) map
        (scopeInstrs, updatedMap) = genInstrs' (VarMap map []) stmts []

genExpr :: VarMap -> AddrImmDI -> Expr -> [Instruction]
genExpr map addr expr = case expr of (NumExp val) -> genNum addr val
                                     (BooExp val) -> genBool addr val
                                     (VarExp val) -> genVar map addr val
                                     (NumCalc ex1 op ex2) -> genNumCalc map addr ex1 op ex2
                                     (BooCalc ex1 op ex2) -> genCalc map addr ex1 (mapBoolOp op) ex2
                                     (CondExp ex1 op ex2) -> genCalc map addr ex1 (mapOrdOp op) ex2
                                     (Parens ex) -> genExpr map addr ex

genNum :: AddrImmDI -> Integer -> [Instruction]
genNum addr num = [ Load (ImmValue $ fromInteger $ num) regA
                  , Store regA addr]

genBool :: AddrImmDI -> Bool -> [Instruction]
genBool addr bool = [ Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA
                    , Store regA addr]

genVar :: VarMap -> AddrImmDI -> String -> [Instruction]
genVar map addr var =  [ Load (findVarMem var map) regA
                       , Store regA addr]

genNumCalc :: VarMap -> AddrImmDI -> Expr -> CalcOp -> Expr -> [Instruction]
genNumCalc map addr ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = genCalc map addr ex1 (mapCalcOp op) ex2
    | otherwise =  (genExpr map temp1 ex1) ++
                   (genExpr map temp2 ex2) ++
                   [ Load temp1 regA
                   , Load temp2 regB
                   , Load (ImmValue 0) regC
                   , Compute Lt regA regB regD
                   , Branch regD (Rel 4)
                   , Compute Sub regA regB regA
                   , Compute Incr regC regC regC
                   , Jump (Rel (-4))
                   , Store regC addr
                   ]

genCalc :: VarMap -> AddrImmDI -> Expr -> Operator -> Expr -> [Instruction]
genCalc map addr ex1 op ex2 = (genExpr map temp1 ex1) ++
                              (genExpr map temp2 ex2) ++
                              [ Load temp1 regA
                              , Load temp2 regB
                              , Compute op regA regB regC
                              , Store regC addr
                              ]

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