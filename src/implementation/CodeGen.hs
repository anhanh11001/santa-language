module CodeGen where

import Sprockell
import LangDef
import Debug.Trace

data VarMap = VarMap VarMap [VarMem]
            | NullMap deriving (Eq, Show)
data VarMem = VarMem String AddrImmDI
            | MapMem VarMap deriving (Eq, Show)

temporaryAddresses = 2

findVarMem :: String -> VarMap -> AddrImmDI
findVarMem varNem NullMap = error ("Cannot find " ++ varNem)
findVarMem varMem (VarMap motherMap []) = findVarMem varMem motherMap
findVarMem varMem (VarMap motherMap ((VarMem memName memAddr):y)) = if (varMem == memName)
  then memAddr else findVarMem varMem (VarMap motherMap y)

storeVarMem :: String -> AddrImmDI -> VarMap -> VarMap
storeVarMem varName addr NullMap = VarMap NullMap [VarMem varName addr]
storeVarMem varName addr (VarMap m mems) = VarMap m (mems ++ [VarMem varName addr])

countMap :: VarMap -> Int
countMap map = case map of NullMap -> 0
                           (VarMap mother mem) -> (countMapD map) + (countMapU mother)

countMapU :: VarMap -> Int
countMapU NullMap = 0
countMapU (VarMap mother mem) = (countMapU mother) + (countMapU' mem)

countMapU' :: [VarMem] -> Int
countMapU' [] = 0
countMapU' (x:y) = case x of (VarMem _ _) -> 1 + (countMapU' y)
                             (MapMem _) -> 0
countMapD :: VarMap -> Int
countMapD NullMap = 0
countMapD (VarMap _ mem) = countMapD' mem

countMapD' :: [VarMem] -> Int
countMapD' [] = 0
countMapD' (x:y) = case x of (VarMem _ _) -> 1 + (countMapD' y)
                             (MapMem m) -> countMapD m

genCode :: Program -> [Instruction]
genCode (Program stmts) = (fst (genCode' NullMap stmts [])) ++ [EndProg]

genCode' :: VarMap -> [Stmt] -> [Instruction] -> ([Instruction], VarMap)
genCode' map [] instrs = (instrs, map)
genCode' map (x:y) instrs = genCode' newMap y (instrs ++ instr)
  where (instr, newMap) = genStmt x map

genStmt :: Stmt -> VarMap -> ([Instruction], VarMap)
genStmt stmt map = case stmt of (VarDecStmt (VarDec varType varName val)) -> genVarDec map varName val
                                (VarReDecStmt (VarReDec varName val)) -> (genVarReDec map varName val, map)
                                (WheStmt (Where expr scope)) -> genWhere map expr scope
                                (IfStmt ifStmt) -> genIf map ifStmt
                                (LockStmt lockStmt) -> undefined
                                (ThreadStmt thrStmt) -> undefined

genVarDec :: VarMap -> String -> Expr -> ([Instruction], VarMap)
genVarDec varMap varName expr = (instrs, newVarMap)
  where instrs = genExpr varMap addr expr
        newVarMap = storeVarMem varName addr varMap
        addr = DirAddr (countMap varMap + 1 + temporaryAddresses)

genVarReDec :: VarMap -> String -> Expr -> [Instruction]
genVarReDec map varName expr = genExpr map (findVarMem varName map) expr

genWhere :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genWhere map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA,
                                      Load (DirAddr 1) regF,
                                      Compute Sub regA regF regF,
                                      Branch regF (Rel (length scopeInstrs + 2)) ] ++
                                    scopeInstrs ++
                                    [ Jump (Rel (- (length scopeInstrs) - 4 - (length exprInstrs))) ]
                                    ,updatedMap)
  where exprInstrs = genExpr map (DirAddr 1) expr
        (scopeInstrs, updatedMap) = genCode' (VarMap map []) stmts []

genIf :: VarMap -> If -> ([Instruction], VarMap)
genIf map ifStmt = case ifStmt of (IfOne expr scope1 scope2) -> genIfOne map expr scope1 scope2
                                  (IfTwo expr scope) -> genIfTwo map expr scope

genIfOne :: VarMap -> Expr -> Scope -> Scope -> ([Instruction], VarMap)
genIfOne map expr (Scope stmts1) (Scope stmts2) = ( exprInstrs ++
                                                    [ Load (ImmValue 1) regA,
                                                      Load (DirAddr 1) regF,
                                                      Compute Sub regA regF regF,
                                                      Branch regF (Rel (length scopeInstrs1 + 2)) ] ++
                                                    scopeInstrs1 ++
                                                    [ Jump (Rel (length scopeInstrs2 + 1)) ] ++
                                                    scopeInstrs2
                                                  , updatedMap2 )
  where exprInstrs = genExpr map (DirAddr 1) expr
        (scopeInstrs1, updatedMap1) = genCode' (VarMap map []) stmts1 []
        (scopeInstrs2, updatedMap2) = genCode' (VarMap updatedMap1 []) stmts2 []

genIfTwo :: VarMap -> Expr -> Scope -> ([Instruction], VarMap)
genIfTwo map expr (Scope stmts) = ( exprInstrs ++
                                    [ Load (ImmValue 1) regA
                                    , Load (DirAddr 1) regF
                                    , Compute Sub regA regF regF
                                    , Branch regF (Rel (length scopeInstrs + 1))] ++
                                    scopeInstrs
                                  , updatedMap)
  where exprInstrs = genExpr map (DirAddr 1) expr
        (scopeInstrs, updatedMap) = genCode' (VarMap map []) stmts []

genExpr :: VarMap -> AddrImmDI -> Expr -> [Instruction]
genExpr map addr expr = case expr of (NumExp val) -> genNum addr val
                                     (BooExp val) -> genBool addr val
                                     (VarExp val) -> genVar map addr val
                                     (NumCalc ex1 op ex2) -> genNumCalc map addr ex1 op ex2
                                     (BooCalc ex1 op ex2) -> genCalc map addr ex1 (mapBoolOp op) ex2
                                     (CondExp ex1 op ex2) -> genCalc map addr ex1 (mapOrdOp op) ex2
                                     (Parens ex) -> genExpr map addr ex

genNum :: AddrImmDI -> Integer -> [Instruction]
genNum addr num = [Load (ImmValue $ fromInteger $ num) regA,
                     Store regA addr]

genBool :: AddrImmDI -> Bool -> [Instruction]
genBool addr bool = [Load (ImmValue $ (\x -> if x then 1 else 0) bool) regA,
                       Store regA addr]

genVar :: VarMap -> AddrImmDI -> String -> [Instruction]
genVar map addr var =  [Load (findVarMem var map) regA,
                          Store regA addr]

genNumCalc :: VarMap -> AddrImmDI -> Expr -> CalcOp -> Expr -> [Instruction]
genNumCalc map addr ex1 op ex2
    | op `elem` [AddOp, SubOp, Mult] = genCalc map addr ex1 (mapCalcOp op) ex2
    | otherwise =  (genExpr map (DirAddr 1) ex1) ++
                   (genExpr map (DirAddr 2) ex2) ++
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

genCalc :: VarMap -> AddrImmDI -> Expr -> Operator -> Expr -> [Instruction]
genCalc map addr ex1 op ex2 = (genExpr map (DirAddr 1) ex1) ++
                              (genExpr map (DirAddr 2) ex2) ++
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