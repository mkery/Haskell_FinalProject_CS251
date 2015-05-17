--following along https://wiki.haskell.org/Parsing_a_simple_imperative_language

module AdventureParser where

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

phrase :: Parser [String]
phrase = do
	words <- sepBy1 (many1 letter) (skipMany1 $ space <|> char ',')
	return words


pr_walk_to :: Parser (String, [Maybe String])
pr_walk_to = do
  reserved "walk to"
  x <- optionMaybe $ many1 anyChar
  return ("walk_to", [x])

pr_look :: Parser (String, [Maybe String])
pr_look = do
	reserved "look"
	x <- optionMaybe $ many1 anyChar
	return ("look", [x])

pr_help :: Parser (String, [Maybe String])
pr_help = do
	reserved "help"
	x <- optionMaybe $ many1 anyChar
	return ("help", [x])

pr_open :: Parser (String, [Maybe String])
pr_open = do
	reserved "open"
	x <- optionMaybe $ many1 anyChar
	return ("open", [x])

pr_putin :: Parser (String, [Maybe String])
pr_putin = do
	reserved "put"
	x <- optionMaybe $ manyTill anyChar space
	reservedOp "in"
	y <- optionMaybe $ many1 anyChar
	return ("putin", [x,y])

pr_pocket :: Parser (String, [Maybe String])
pr_pocket = do
	reserved "pocket"
	x <- optionMaybe $ many1 anyChar
	return ("pocket", [x])

expr :: Parser (String, [Maybe String])
expr = pr_walk_to <|> pr_look <|> pr_help <|> pr_open <|> pr_putin <|> pr_pocket

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> (String, [Maybe String])
parseExpr t = 
  case parse (allOf expr) "" t of
    Left err -> error (show err)
    Right ast -> ast


