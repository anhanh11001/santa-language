module BParser where

import LangDef
import PComb
import Text.ParserCombinators.Parsec
import TypeScope

transform :: String -> a -> Parser a
transform x f = (\m -> f) <$> symbol x

varType :: Parser VarType
varType = spaces >> varType' <?> "Fail on varType"
varType' :: Parser VarType
varType' = transform "num" Num
       <|> transform "bool" Boo
       <|> transform "char" Char
       <|> transform "Str" Str

boolOp :: Parser BoolOp
boolOp = spaces >> boolOp' <?> "Fail on boolOp"
boolOp' :: Parser BoolOp
boolOp' = transform "&&" AndOp
      <|> transform "||" OrOp

ordOp :: Parser OrdOp
ordOp = spaces >> ordOp' <?> "Fail on ordOp"
ordOp' :: Parser OrdOp
ordOp' = try (transform "<=" LE)
     <|> try (transform ">=" ME)
     <|> try (transform "==" E)
     <|> transform "<" L
     <|> transform ">" M
     <|> transform "!=" NE

condition :: Parser Expr
condition = try (BooCalc <$> term <*> boolOp <*> expr)
         <|> try (CondExp <$> factor <*> ordOp <*> term)
         <|> try (BooExp <$> (transform "true" True <|> transform "false" False))
         <|> try (VarExp <$> identifier)
         <|> Parens <$> (parens condition)
         <?> "Fail on condition"

expr :: Parser Expr
expr = spaces >> expr' <?> "Fail on expr"
expr' :: Parser Expr
expr' = try (BooCalc <$> term <*> boolOp <*> expr)
    <|> term3

term3 :: Parser Expr 
term3 = try (CondExp <$>  term2 <*> ordOp <*> term3)
     <|> term2
     
term2 :: Parser Expr
term2 = try (NumCalc <$> term <*> (add <|> sub) <*> term2)
     <|> term

term :: Parser Expr
term = try (NumCalc <$> factor <*> (mul <|> divOp) <*> term)
    <|> factor

factor :: Parser Expr
factor = NumExp <$> integer
      <|> Parens <$> parens expr
      <|> VarExp <$> identifier
      <|> BooExp <$> (transform "true" True <|> transform "false" False)

ifP :: Parser If
ifP = spaces >> ifP' <?> "Fail on ifP"
ifP' :: Parser If
ifP' = try (IfOne <$> ((reserved "santa_check") *> (parens condition))
             <*> ((reserved "then_he_do") *> (braces scope))
             <*> ((reserved "otherwise_he_do") *> (braces scope)))
   <|> IfTwo <$> ((reserved "santa_check") *> (parens condition))
             <*> ((reserved "then_he_do") *> (braces scope))

whereP :: Parser Where
whereP = spaces >> whereP' <?> "Fail on whereP"
whereP' :: Parser Where
whereP' = Where <$> ((reserved "santa_go_to_factory_when")
                *> (parens condition))
               <*> braces scope

varReDec :: Parser VarReDec
varReDec = spaces >> varReDec' <?> "Fail on varReDec"
varReDec' :: Parser VarReDec
varReDec' = VarReDec <$> ((reserved "santa_change_gift") *> identifier)
                     <*> (symbol "=" *> expr)

varDec :: Parser VarDec
varDec = spaces >> varDec'
varDec' :: Parser VarDec
varDec' = VarDec <$> ((reserved "santa_make_gift") *> varType)
                <*> identifier
                <*> (symbol "=" *> expr)

lock :: Parser Lock
lock = spaces >> lock' <?> "Fail on lock"
lock' :: Parser Lock
lock' =  LckCreate <$> ((reserved "santa_lock_create") *> identifier)
    <|> LckLock <$> ((reserved "santa_lock") *> identifier)
    <|> LckUnlock <$> ((reserved "santa_unlock") *> identifier)

thread :: Parser Thread
thread = spaces >> thread' <?> "Fail on thread"
thread' :: Parser Thread
thread' = ThrCreate <$> (reserved "christmas_create" *> identifier) <*> (braces scope)
      <|> ThrStart <$> (reserved "christmas_start" *> identifier)
      <|> ThrStop <$> (reserved "christmas_stop" *> identifier)

stmt :: Parser Stmt
stmt =  VarDecStmt <$> varDec
    <|> VarReDecStmt <$> varReDec
    <|> WheStmt <$> whereP
    <|> IfStmt <$> ifP
    <|> LockStmt <$> lock
    <|> ThreadStmt <$> thread
    <?> "Fail on stmt"

scope :: Parser Scope
scope = Scope <$> (endBy stmt (symbol ";")) <?> "Fail on scope"

program :: Parser Program
program = Program <$> (endBy stmt (symbol ";")) <?> "Fail on program"

calcOp :: String -> Parser CalcOp
calcOp op = spaces >> (calcOp' op) <?> "Fail on calcOp"
calcOp' :: String -> Parser CalcOp
calcOp' op = case op of "+" -> transform op AddOp
                        "-" -> transform op SubOp
                        "*" -> transform op Mult
                        "/" -> transform op Div
                        otherwise -> error "Invalid operator"
add = calcOp "+"
sub = calcOp "-"
mul = calcOp "*"
divOp = calcOp "/"


compile :: String -> Program
compile x = (\(Right x) -> x) (parse program "Error" x)