module CodeGen where

import Sprockell
import LangDef

data VarMap = VarMap [VarMem] deriving (Eq, Show)
data VarMem = VarMem String AddrImmDI deriving (Eq, Show)
-- Example: VarMem a Num

findVarMem :: String -> VarMap -> AddrImmDI
findVarMem varName (VarMap map) = findVarMem' varName map

findVarMem' :: String -> [VarMem] -> AddrImmDI
findVarMem' varName [] = error ("Cannot find " ++ varName)
findVarMem' varName ((VarMem memName memAddr):y) = if (varName == memName)
    then memAddr else findVarMem' varName y

storeVarMem :: String -> AddrImmDI -> VarMap -> VarMap
storeVarMem varName addr (VarMap mems) = VarMap (mems ++ [VarMem varName addr])

nextMapID :: VarMap -> Int
nextMapID (VarMap mem) = length mem

genCode :: Program -> [Instruction]
genCode (Program stmts) = undefined -- (++) [] (map (genStmt ()) stmts)

genStmt :: Stmt -> VarMap -> ([Instruction], VarMap)
genStmt stmt map = case stmt of (VarDecStmt (VarDec varType varName val)) -> genVarDec map varName val
                                (VarReDecStmt (VarReDec varName val)) -> (genVarReDec map varName val, map)
                                (WheStmt (Where expr scope)) -> undefined
                                (IfStmt ifStmt) -> undefined
                                (LockStmt lockStmt) -> undefined
                                (ThreadStmt thrStmt) -> undefined

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec varMap varName expr = (instrs, newVarMap)
  where instrs = storeExpr addr expr
        newVarMap = storeVarMem varName addr varMap
        addr = DirAddr (nextMapID varMap)

genVarReDec :: VarMap -> String -> Expr -> [Instruction]
genVarReDec map varName expr = storeExpr (findVarMem varName map) expr

storeExpr :: AddrImmDI -> Expr -> [Instruction]
storeExpr addr expr = case expr of (NumExp val) -> storeNum addr val
                                   (BooExp val) -> storeBool addr val
                                   (NumCalc ex1 op ex2) -> storeNumCalc addr ex1 op ex2
                                   (BooCalc ex1 op ex2) -> storeCalc addr ex1 (mapBoolOp op) ex2
                                   (CondExp ex1 op ex2) -> storeCalc addr ex1 (mapOrdOp op) ex2
                                   (Parens ex) -> storeExpr addr ex

storeNum :: AddrImmDI -> Integer -> [Instruction]
storeNum addr num = [Load (ImmValue $ fromInteger $ num) regA,
                     Store regA addr]

storeBool :: AddrImmDI -> Bool -> [Instruction]
storeBool addr bool = [Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA,
                       Store regA addr]

storeNumCalc :: AddrImmDI -> Expr -> CalcOp -> Expr -> [Instruction]
storeNumCalc addr ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = storeCalc addr ex1 (mapCalcOp op) ex2
    | otherwise =  [ Load (getExprVal ex1) regA
                   , Load (getExprVal ex2) regB
                   , Load (ImmValue 0) regC
                   , Compute Lt regA regB regD
                   , Branch regD (Rel 4)
                   , Compute Sub regA regB regA
                   , Compute Incr regC regC regC
                   , Jump (Rel (-4))
                   , Store regC addr
                   ]

storeCalc :: AddrImmDI -> Expr -> Operator -> Expr -> [Instruction]
storeCalc addr ex1 op ex2 = [ Load (getExprVal ex1) regA
                            , Load (getExprVal ex2) regB
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

getExprVal :: Expr -> AddrImmDI
getExprVal expr = case expr of (NumExp x) -> ImmValue $ fromInteger $ x
