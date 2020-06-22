module PComb where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Char

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = letter
            , Token.reservedNames   = [ "santa_make_gift",
                                        "santa_change_gift",
                                        "santa_check", "then_he_do", "otherwise_he_do",
                                        "santa_go_to_factory_when",
                                        "christmas_create", "christmas_start", "christmas_stop",
                                        "santa_lock_create", "santa_lock", "santa_unlock",
                                        "num", "bool", "char", "str",
                                        "true", "false"
                                      ]
            , Token.reservedOpNames = [ "||", "&&",
                                        "<", "==", ">", ">=", "<=", "!=",
                                        "+", "-", "*", "//",
                                        ";", "="
                                      ]
            , caseSensitive = True
            }

-- Create lexer (=tokenizer) for your language
lexer = Token.makeTokenParser languageDef

-- Create functions for all types of tokens
identifier :: Parser String
identifier = spaces >> Token.identifier lexer

integer :: Parser Integer
integer = spaces >> Token.integer lexer

parens :: Parser a -> Parser a
parens x = spaces >> (Token.parens lexer) x

braces :: Parser a -> Parser a
braces x = spaces >> (Token.braces lexer) x

doubleQuote :: Parser a -> Parser a
doubleQuote parser = spaces >> (quote *> parser) <* quote
  where quote = symbol "\""

symbol :: String -> Parser String
symbol x = spaces >> (Token.symbol lexer) x

reserved :: String -> Parser ()
reserved x= spaces >> (Token.reserved lexer) x



