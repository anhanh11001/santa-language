module BParser where

import LangDef
import PComb
import Text.ParserCombinators.Parsec

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
boolOp' = transform "&&" And
      <|> transform "||" Or

ordOp :: Parser OrdOp
ordOp = spaces >> ordOp' <?> "Fail on ordOp"
ordOp' :: Parser OrdOp
ordOp' = transform "<" L
     <|> transform ">" M
     <|> transform "==" E
     <|> transform "<=" LE
     <|> transform ">=" ME
     <|> transform "!=" NE


condition :: Parser Condition
condition = Condition <$> expr <*> ordOp <*> expr
         <?> "Fail on condition"

expr :: Parser Expr
expr = spaces >> expr' <?> "Fail on expr"

expr' :: Parser Expr
expr' = NumExp <$> integer
    <|> Cond <$> condition
    <|> NumCalc <$> expr <*> calcOp <*> expr
    <|> BooExp <$> (transform "true" True <|> transform "false" False)
    <|> BooCalc <$> expr <*> boolOp <*> expr
    <|> VarExp <$> identifier
    <|> Parens <$> parens expr

ifP :: Parser If
ifP = spaces >> ifP' <?> "Fail on ifP"
ifP' :: Parser If
ifP' = IfOne <$> ((reserved "santa_check") *> (parens condition))
             <*> ((reserved "then_he_do") *> (braces scope))
             <*> ((reserved "otherwise_he_do") *> (braces scope))
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
scope = Scope <$> (sepBy stmt (symbol ";")) <?> "Fail on scope"

program :: Parser Program
program = Program <$> (sepBy stmt (symbol ";")) <?> "Fail on program"

calcOp :: Parser CalcOp
calcOp = spaces >> calcOp' <?> "Fail on calcOp"
calcOp' :: Parser CalcOp
calcOp' =  transform "+" Add
      <|> transform "-" Sub
      <|> transform "*" Mult
      <|> transform "/" Div