module CodeGen where

import Sprockell
import LangDef
import Data.Char
import Data.List

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This file contains functions generating Spril Instructions from the Program tree
-- * See function: genCode
-- ========================================================================================================

-- ==========================================================================================================
-- CodeGen data class/data structure to store information about the program and used to help generating instructions
-- ==========================================================================================================

type LocalAddr = Int                                                          -- from 0 to beyond
type SharedAddr = Int                                                         -- from 4 to 7
type ThrAddr = Int                                                            -- from 1 to 3 (0 is the default shared memory for main thread)

data VarMap = VarMap VarMap LocalAddr SharedAddr ThrAddr [VarMem]             -- Data structure to store variables inside normal scope (main program, if-else/when statements)
            | ThrMap VarMap VarMap LocalAddr [VarMem]                         -- Data structure to store variables inside thread scope (Thread cannot initialize a new global variable and a new thread)
            | NullMap                                                         -- Empty map. For example, mother scope of main program is NullMap
            deriving (Eq, Show)
data VarMem = LocalMem String AddrImmDI
            | SharedMem String AddrImmDI
            | MapMem VarMap
            deriving (Eq, Show)

-- ==========================================================================================================
-- Functions to take the next number of address (Local Memory 0 - 31) (Thread 1 - 3) (Shared Memory 4 - 7)
-- ==========================================================================================================

eAddr = -1                                                                    -- Empty Addr
startLA = 0                                                                   -- Starting address of local variable (LA - Local address)
startSA = 4                                                                   -- Starting address of global variable (SA - Shared address)
startTA = 1                                                                   -- Starting address of thread (Can have 3 thread + main thread) (TA - Thread address)

mapNextLA (VarMap a b c d e) = VarMap a (nextLAAddr b) c d e                  -- Get the map that update the local address
mapNextLA NullMap = VarMap NullMap startLA eAddr eAddr []                     --
mapNextLA (ThrMap a b c d) = ThrMap a b (nextLAAddr c) d                      --
mapNextSA (VarMap a b c d e) = VarMap a b (nextSAAddr c) d e                  -- Get the map that update the shared address
mapNextSA NullMap = VarMap NullMap eAddr startSA eAddr []                     --
mapNextTA (VarMap a b c d e) = VarMap a b c (nextTAAddr d) e                  -- Get the map that update the thread address
mapNextTA NullMap = VarMap NullMap eAddr eAddr startTA []                     --

nextLA (VarMap _ a _ _ _) = DirAddr (nextLAAddr a)                            -- Get next local address
nextLA NullMap = DirAddr startLA                                              --
nextLA (ThrMap _ _ a _) = DirAddr (nextLAAddr a)                              --
nextSA (VarMap _ _ a _ _ ) = DirAddr (nextSAAddr a)                           -- Get next shared address
nextSA NullMap = DirAddr startSA                                              --
nextTA (VarMap _ _ _ a _) = DirAddr (nextTAAddr a)                            -- Get next thread address
nextTA NullMap = DirAddr startTA                                              --

getLA (VarMap _ a _ _ _) = DirAddr a                                          -- Get the current local address
getLA NullMap = DirAddr startLA                                               --
getLA (ThrMap _ _ a _) = DirAddr a                                            --
getSA (VarMap _ _ a _ _) = DirAddr a                                          -- Get the current shared address
getSA NullMap = DirAddr startSA                                               --
getTA (VarMap _ _ _ a _) = DirAddr a                                          -- Get the current thread address
getTA NullMap = DirAddr startTA                                               --

lAddr (VarMap _ a _ _ _) = a                                                  -- Get the number of the current local address
lAddr (ThrMap _ _ a _) = a                                                    --
sAddr (VarMap _ _ a _ _) = a                                                  -- Get the number of the current shared address
tAddr (VarMap _ _ _ a _) = if a == eAddr then 0 else a                        -- Get the number of the current thread address

nextTAAddr a  -- From 1 to 3
 | a == eAddr = startTA
 | a >= 3 = error "There can only be 3 additional threads"
 | otherwise = a + 1

nextLAAddr a  -- From 0 to 31
  | a == eAddr = startLA
  | a >= 31 = error "There can only be maximum of 32 local memories in this thread"
  | otherwise = a + 1

nextSAAddr a  -- From 4 to 7
  | a == eAddr = startSA
  | a >= 7 = error "There can only be maximum of 4 shared memories used in this program"
  | otherwise = a + 1

-- ==========================================================================================================
-- Main function used to generate instructions for a program
-- ==========================================================================================================

genCode :: Program -> [[Instruction]]                                         -- Generate instructions code from the Program tree
genCode prog = genRP instrs (countThrNum prog + 1) []                         --
  where (instrs, map) = genInstrs prog                                        --
        genRP _ 0 x = x                                                       --
        genRP l num x = genRP l (num - 1) (l : x)                             --

genInstrs :: Program -> ([Instruction], VarMap)                               -- Generate instructions code and the created data structure
genInstrs (Program stmts) = (instrs ++ [EndProg], map)                        -- from the Program tree
  where (instrs, map) = genInstrs' NullMap stmts []                           --
                                                                              --
genInstrs' :: VarMap -> [Stmt] -> [Instruction] -> ([Instruction], VarMap)    --
genInstrs' map [] instrs = (instrs, map)                                      --
genInstrs' map (x:y) instrs = genInstrs' newMap y (instrs ++ instr)           --
  where (instr, newMap) = genStmt x map                                       --

-- ==========================================================================================================
-- Function used to generate instructions for the thread scope (Can only used local variables inside the scope or global variables)
-- ==========================================================================================================

genInstrsThr :: VarMap -> Scope -> ([Instruction], VarMap)                                -- Generate instructions code and
genInstrsThr NullMap (Scope stmts) = (instrs, NullMap)
  where instrs = fst $ genInstrsThr' (ThrMap NullMap NullMap eAddr []) stmts []
genInstrsThr (VarMap m l s t mem) (Scope stmts) = (instrs, (VarMap m l s t mem))           -- the created data structure
  where instrs = fst $ genInstrsThr' (ThrMap (VarMap m l s t mem) NullMap l []) stmts []   -- from the thread scope

genInstrsThr' :: VarMap -> [Stmt] -> [Instruction] -> ([Instruction], VarMap)             --
genInstrsThr' map [] instrs = (instrs, map)                                               --
genInstrsThr' map (x:y) instrs = genInstrsThr' map1 y (instrs ++ instr)                   --
  where (instr, map1) = genThrStmt x map                                                  --

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

genThrStmt :: Stmt -> VarMap -> ([Instruction], VarMap)                                                 -- Cannot create a new
genThrStmt stmt map = case stmt of VarDecStmt (VarDec _ varName val) -> genVarDec map varName val       -- global variable, a
                                   (VarReDecStmt (VarReDec varName val)) -> genVarReDec map varName val -- new lock, a new thread
                                   (WheStmt (Where expr scope)) -> genWhere map expr scope              -- from the a thread
                                   (IfStmt ifStmt) -> genIf map ifStmt                                  --
                                   (PrintStmt varName) -> genVarPrint map varName                       --
                                   (LockStmt lockStmt) -> genLock map lockStmt                          --
                                   ExitStmt -> (genExit, map)                                           --

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
        (scopeInstrs, map2) = genInstrs' (VarMap map (lAddr map) (sAddr map) (tAddr map) []) stmts []
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
        (scopeInstrs1, map2) = genInstrs' (VarMap map (lAddr map) (sAddr map) (tAddr map) []) stmts1 []
        map3 = storeVar (MapMem map2) map
        (scopeInstrs2, map4) = genInstrs' (VarMap map3 (lAddr map3) (sAddr map3) (tAddr map3) []) stmts2 []
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
        (scopeInstrs, map2) = genInstrs' (VarMap map1 (lAddr map) (sAddr map) (tAddr map) []) stmts []
        map3 = storeVar (MapMem map2) map1

genThread :: VarMap -> Thread -> ([Instruction], VarMap)
genThread map thr = case thr of (ThrStart thrName scope) -> genThrStart map thrName scope
                                (ThrStop thrName) -> genThrStop map thrName

genThrStart :: VarMap -> String -> Scope -> ([Instruction], VarMap)
genThrStart map thrName scope = ([ Jump (Rel 4)
                                 , Load (ImmValue 0) regA
                                 , WriteInstr regA (getTA map2)
                                 , EndProg
                                 , Load (ImmValue ((\(DirAddr x) -> x) $ getTA map2)) regA
                                 , Compute NEq regA regSprID regA
                                 , Branch regA (Rel (length instrs + 4))
                                 ] ++
                                 [ Load (ImmValue 1) regA
                                 , WriteInstr regA (getTA map2) ] ++
                                 instrs ++
                                 [ Jump (Rel ((- length instrs) - 8)) ]
                                 , map3)
  where (instrs, map1) = genInstrsThr map scope
        map2 = mapNextTA map1
        map3 = storeVar (SharedMem thrName (getTA map2)) map2

genThrStop :: VarMap -> String -> ([Instruction], VarMap)
genThrStop map thrName = ( loop ++ [ Branch regA (Rel (-length loop)) ], map)
  where thrAddr = fst $ findVarMem thrName map
        loop = [ Nop, Nop
               , TestAndSet thrAddr
               , Receive regA
               , Load (ImmValue 0) regB
               , Compute Equal regA regB regA ]

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
    | otherwise =  (instrs1 ++                                                      -- This is the section for soft division
                   instrs2 ++
                   [ Load (getLA map1) regA
                   , Load (getLA map2) regB
                   , Load (ImmValue 0) regE
                   , Compute NEq regB regE regE
                   , Branch regE (Rel (length divideZeroStr + 2))
                   ] ++ divideZeroStr ++
                   [ EndProg
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
                         divideZeroStr = genString "Division by zero error \n"

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

findVarMem :: String -> VarMap -> (AddrImmDI, Bool)                                                             -- Find a variable address and whether it is
findVarMem varMem map = findVarMem' False varMem map                                                            -- local - false or global - true

findVarMem' :: Bool -> String -> VarMap -> (AddrImmDI, Bool)                                                    --
findVarMem' shared varMem NullMap = error ("Cannot find " ++ varMem)                                            --
findVarMem' shared varMem (VarMap motherMap _ _ _ []) = findVarMem' shared varMem motherMap                     --
findVarMem' shared varMem (VarMap motherMap a s t (x:y)) =                                                      --
  case x of (LocalMem memName memAddr) -> if (varMem == memName && not shared) then (memAddr, False) else next  --
            (SharedMem memName memAddr) -> if (varMem == memName) then (memAddr, True) else next                --
            otherwise -> next                                                                                   --
  where next = findVarMem' shared varMem (VarMap motherMap a s t y)                                             --
findVarMem' shared varMem (ThrMap map NullMap _ []) = findVarMem' True varMem map                               --
findVarMem' shared varMem (ThrMap map mother t (x:y)) =                                                         --
  case x of (LocalMem memName memAddr) -> if (varMem == memName && not shared) then (memAddr, False) else next  --
            (SharedMem memName memAddr) -> if (varMem == memName) then (memAddr, True) else next                --
            otherwise -> next                                                                                   --
  where next = findVarMem' shared varMem (ThrMap map mother t y)                                                --

storeVar :: VarMem -> VarMap -> VarMap                                                                          -- Store new element to the map
storeVar (LocalMem str addr) NullMap =                                                                          -- (data structure that keeps
  VarMap NullMap (dirToAddr addr) eAddr eAddr [LocalMem str addr]                                               -- all variables)
storeVar (SharedMem str addr) NullMap =                                                                         --
  VarMap NullMap eAddr (dirToAddr addr) eAddr [SharedMem str addr]                                              --
storeVar (MapMem (VarMap mother a s t l)) NullMap =                                                             --
  VarMap NullMap a s t [MapMem (VarMap mother a s t l)]                                                         --

storeVar (LocalMem str addr) (VarMap m a s t l) = VarMap m a s t (l ++ [LocalMem str addr])                     --
storeVar (SharedMem str addr) (VarMap m a s t l) = VarMap m a s t (l ++ [SharedMem str addr])                   --
storeVar (MapMem (VarMap m1 a1 s1 t1 l1)) (VarMap m2 a2 s2 t2 l2) =                                             --
  VarMap m2 a1 s1 t1 (l2 ++ [MapMem (VarMap m1 a1 s1 t1 l1)])                                                   --

storeVar (LocalMem str addr) (ThrMap mainThr mother a l) =                                                      --
  ThrMap mainThr mother a (l ++ [LocalMem str addr])                                                            --
storeVar (SharedMem str addr) (ThrMap mainThr mother a l) =                                                     --
  error "Cannot create shared variable in a new thread"                                                         --
storeVar (MapMem (VarMap m1 a1 s1 t1 l1)) (ThrMap mainThr mother a l) =                                         --
  ThrMap mainThr mother a1 (l ++ [MapMem (VarMap m1 a1 s1 t1 l1)])                                              --

dirToAddr addr = case addr of (DirAddr x) -> x                                                                  -- Get the address number of the DirAddr
                              otherwise -> error "Invalid addr"                                                 --

countThrNum :: Program -> Integer                                                                               -- Count the number of thread used inside
countThrNum (Program stmts) = countThrNum' stmts 0                                                              -- a program (excluding the main thread)

countThrNum' :: [Stmt] -> Integer -> Integer                                                                    --
countThrNum' [] x = x                                                                                           --
countThrNum' (x:y) n = case x of (ThreadStmt (ThrStart _ _)) -> countThrNum' y (n+1)                            --
                                 otherwise -> countThrNum' y n                                                  --

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

-- ==========================================================================================================
-- Generate strings instruction for debugging purpose only
-- ==========================================================================================================

genString :: String -> [Instruction]
genString str = concat $ map writeChar str
  where writeChar c = [ Load (ImmValue $ ord c) regA , WriteInstr regA charIO ]
