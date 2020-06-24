module CodeGen where

import Sprockell
import LangDef

data VarMap = VarMap VarMap [VarMem]
            | NullMap deriving (Eq, Show)
data VarMem = VarMem String AddrImmDI
            | MapMem VarMap deriving (Eq, Show)
-- Example: VarMem a Num

temporaryAddressSize = 2

findVarMem :: String -> VarMap -> AddrImmDI
findVarMem varName (VarMap _ map) = findVarMem' varName map

findVarMem' :: String -> [VarMem] -> AddrImmDI
findVarMem' varName [] = error ("Cannot find " ++ varName)
findVarMem' varName ((VarMem memName memAddr):y) = if (varName == memName)
    then memAddr else findVarMem' varName y

storeVarMem :: String -> AddrImmDI -> VarMap -> VarMap
storeVarMem varName addr NullMap = VarMap NullMap [VarMem varName addr]
storeVarMem varName addr (VarMap m mems) = VarMap m (mems ++ [VarMem varName addr])

countMap :: VarMap -> Int
countMap map = case map of NullMap -> 0
                           (VarMap _ mem) -> countMap' mem

countMap' :: [VarMem] -> Int
countMap' [] = 0
countMap' (x:y) = case x of (VarMem _ _) -> 1 + (countMap' y)
                            (MapMem m) -> countMap m

varEx = Program [ VarDecStmt (VarDec Num "a" (NumExp 12))
                , VarDecStmt (VarDec Boo "b" (BooExp True))
                , VarReDecStmt (VarReDec "a" (NumCalc (NumExp 13) AddOp (VarExp "a")))
                ]

genCode :: Program -> [Instruction]
genCode (Program stmts) = genCode' NullMap stmts []

genCode' :: VarMap -> [Stmt] -> [Instruction] -> [Instruction]
genCode' _ [] instrs = instrs
genCode' map (x:y) instrs = genCode' newMap y (instrs ++ instr)
  where (instr, newMap) = genStmt x map

genStmt :: Stmt -> VarMap -> ([Instruction], VarMap)
genStmt stmt map = case stmt of (VarDecStmt (VarDec varType varName val)) -> genVarDec map varName val
                                (VarReDecStmt (VarReDec varName val)) -> (genVarReDec map varName val, map)
                                (WheStmt (Where expr scope)) -> undefined
                                (IfStmt ifStmt) -> undefined
                                (LockStmt lockStmt) -> undefined
                                (ThreadStmt thrStmt) -> undefined

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec varMap varName expr = (instrs, newVarMap)
  where instrs = storeExpr varMap addr expr
        newVarMap = storeVarMem varName addr varMap
        addr = DirAddr (countMap varMap + 1 + temporaryAddressSize)

genVarReDec :: VarMap -> String -> Expr -> [Instruction]
genVarReDec map varName expr = storeExpr map (findVarMem varName map) expr

storeExpr :: VarMap -> AddrImmDI -> Expr -> [Instruction]
storeExpr map addr expr = case expr of (NumExp val) -> storeNum addr val
                                       (BooExp val) -> storeBool addr val
                                       (VarExp val) -> storeVar map addr val
                                       (NumCalc ex1 op ex2) -> storeNumCalc map addr ex1 op ex2
                                       (BooCalc ex1 op ex2) -> storeCalc map addr ex1 (mapBoolOp op) ex2
                                       (CondExp ex1 op ex2) -> storeCalc map addr ex1 (mapOrdOp op) ex2
                                       (Parens ex) -> storeExpr map addr ex

storeNum :: AddrImmDI -> Integer -> [Instruction]
storeNum addr num = [Load (ImmValue $ fromInteger $ num) regA,
                     Store regA addr]

storeBool :: AddrImmDI -> Bool -> [Instruction]
storeBool addr bool = [Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA,
                       Store regA addr]

storeVar :: VarMap -> AddrImmDI -> String -> [Instruction]
storeVar map addr var =  [Load (findVarMem var map) regA,
                          Store regA addr]

storeNumCalc :: VarMap -> AddrImmDI -> Expr -> CalcOp -> Expr -> [Instruction]
storeNumCalc map addr ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = storeCalc map addr ex1 (mapCalcOp op) ex2
    | otherwise =  (storeExpr map (DirAddr 1) ex1) ++
                   (storeExpr map (DirAddr 2) ex2) ++
                   [ Load (DirAddr 1) regA
                   , Load (DirAddr 2) regB
                   , Load (ImmValue 0) regC
                   , Compute Lt regA regB regD
                   , Branch regD (Rel 4)
                   , Compute Sub regA regB regA
                   , Compute Incr regC regC regC
                   , Jump (Rel (-4))
                   , Store regC addr
                   ]

storeCalc :: VarMap -> AddrImmDI -> Expr -> Operator -> Expr -> [Instruction]
storeCalc map addr ex1 op ex2 = (storeExpr map (DirAddr 1) ex1) ++
                                (storeExpr map (DirAddr 2) ex2) ++
                                [ Load (DirAddr 1) regA
                                , Load (DirAddr 2) regB
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