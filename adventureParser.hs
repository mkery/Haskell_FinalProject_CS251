--following along https://wiki.haskell.org/Parsing_a_simple_imperative_language

module ParseAdventure (parseExpr) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token




lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef
  where languageDef = emptyDef {
  			  Token.reservedNames   = [ "look", "open", "walk to", "pocket", "put"]
            , Token.reservedOpNames = ["in"]
            }

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String --gonna use for obj names
identifier = Token.identifier lexer

walk_to :: Parser String
walk_to = do
  reserved "walk to"
  x <- identifier
  return x

look :: Parser String
look = do
	reserved "look"
	x <- identifier
	return x	

expr :: Parser String
expr = walk_to <|> look

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> String
parseExpr t = 
  case parse (allOf expr) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast


