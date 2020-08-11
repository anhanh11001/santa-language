module BParser where

import LangDef
import PComb
import Text.ParserCombinators.Parsec

-- ==========================================================================================================
-- FILE DESCRIPTION
-- * This file contains the parser for the program, used to generate the Program tree from stream
-- * See parser: program
-- ==========================================================================================================

-- ==========================================================================================================
-- Parsers of the program
-- ==========================================================================================================

compile :: String -> Program
compile x = (\(Right x) -> x) (parse program "Error" x)

program :: Parser Program
program = Program <$> (endBy stmt (symbol ";")) <?> "Fail on program"

scope :: Parser Scope
scope = Scope <$> (endBy stmt (symbol ";")) <?> "Fail on scope"

thrScope = Scope <$> (endBy thrStmt (symbol ";")) <?> "Fail on scope"

-- ==========================================================================================================
-- Parser of the statements of the program
-- ==========================================================================================================

stmt :: Parser Stmt
stmt =  try (VarDecStmt <$> varDec)
    <|> try (VarDecSpecialStmt <$> varDecSpecial)
    <|> try (VarReDecStmt <$> varReDec)
    <|> try (WheStmt <$> whereP)
    <|> try (IfStmt <$> ifP)
    <|> try (LockStmt <$> lock)
    <|> try (ThreadStmt <$> thread)
    <|> try (PrintStmt <$> (spaces >> ((reserved "santa say") *> identifier)))
    <|> try (fmap (\_ -> ExitStmt) (spaces >> (reserved "santa die")))
    <?> "Fail on stmt"

thrStmt :: Parser Stmt
thrStmt = try (VarDecStmt <$> varDec)
      <|> try (VarReDecStmt <$> varReDec)
      <|> try (WheStmt <$> whereP)
      <|> try (IfStmt <$> ifP)
      <|> try (LockStmt <$> lock)
      <|> try (PrintStmt <$> (spaces >> ((reserved "santa say") *> identifier)))
      <|> try (fmap (\_ -> ExitStmt) (spaces >> (reserved "santa die")))
      <?> "Fail on stmt"

-- ==========================================================================================================
-- Parsers on each statements that can be used in the program
-- ==========================================================================================================

condition :: Parser Expr
condition = try (BooCalc <$> term <*> boolOp <*> expr)
         <|> try (CondExp <$> factor <*> ordOp <*> term)
         <|> try (BooExp <$> (transform "true" True <|> transform "false" False))
         <|> try (VarExp <$> identifier)
         <|> Parens <$> (parens condition)
         <?> "Fail on condition"

ifP :: Parser If
ifP = spaces >> ifP' <?> "Fail on ifP"
ifP' :: Parser If
ifP' = try (IfOne <$> ((reserved "santa check") *> (parens condition))
             <*> ((reserved "then he do") *> (braces scope))
             <*> ((reserved "otherwise he do") *> (braces scope)))
   <|> IfTwo <$> ((reserved "santa check") *> (parens condition))
             <*> ((reserved "then he do") *> (braces scope))

whereP :: Parser Where
whereP = spaces >> whereP' <?> "Fail on whereP"
whereP' :: Parser Where
whereP' = Where <$> ((reserved "santa go to factory when") *> (parens condition))
               <*> braces scope

varReDec :: Parser VarReDec
varReDec = spaces >> varReDec' <?> "Fail on varReDec"
varReDec' :: Parser VarReDec
varReDec' = VarReDec <$> ((reserved "santa change gift") *> identifier)
                     <*> (symbol "=" *> expr)

varDec :: Parser VarDec
varDec = spaces >> (VarDec <$> ((reserved "santa make gift") *> varType) <*> identifier <*> (symbol "=" *> expr))

varDecSpecial :: Parser VarDecSpecial
varDecSpecial = spaces >> (VarDecSpecial <$> ((reserved "santa make special gift") *> varType) <*> identifier <*> (symbol "=" *> expr))

lock :: Parser Lock
lock = spaces >> lock' <?> "Fail on lock"
lock' :: Parser Lock
lock' =  LckCreate <$> ((reserved "santa lock create") *> identifier)
    <|> LckLock <$> ((reserved "santa lock") *> identifier)
    <|> LckUnlock <$> ((reserved "santa unlock") *> identifier)

thread :: Parser Thread
thread = spaces >> thread' <?> "Fail on thread"
thread' :: Parser Thread
thread' = ThrStart <$> (reserved "christmas start" *> identifier) <*> (braces thrScope)
      <|> ThrStop <$> (reserved "christmas stop" *> identifier)

-- ==========================================================================================================
-- Parsers for expressions used in the program
-- The parsing priority orders are div/mult to add/sub to comparison to boolean logic
-- ==========================================================================================================

expr :: Parser Expr
expr = spaces >> expr' <?> "Fail on expr"
expr' :: Parser Expr
expr' = chainl1 term3 op
  where op = f AndOp "&&" <|> f OrOp "||"
        f operator sym = try $ (\ex1 ex2 -> BooCalc ex1 operator ex2) <$ symbol sym

term3 = chainl1 term2 op
  where op = f LE "<=" <|> f ME ">=" <|> f NE "!=" <|> f L "<" <|> f M ">" <|> f E "=="
        f operator sym = try $ (\ex1 ex2 -> CondExp ex1 operator ex2) <$ symbol sym

term2 = chainl1 term (fNum AddOp "+" <|> fNum SubOp "-")
term = chainl1 factor (fNum Mult "*" <|> fNum Div "/")
fNum operator sym = try $ (\ex1 ex2 -> NumCalc ex1 operator ex2) <$ symbol sym

factor :: Parser Expr
factor = NumExp <$> integer
      <|> Parens <$> parens expr
      <|> VarExp <$> identifier
      <|> BooExp <$> (transform "true" True <|> transform "false" False)

-- ==========================================================================================================
-- Parsers for operators and types
-- ==========================================================================================================

transform :: String -> a -> Parser a
transform x f = (\m -> f) <$> symbol x

varType :: Parser VarType
varType = spaces >> varType' <?> "Fail on varType"
varType' :: Parser VarType
varType' = transform "num" Num
       <|> transform "bool" Boo

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