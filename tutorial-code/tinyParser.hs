import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

{-
following tutorial at http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
to figure out how to make a simple parser for my adventure game
-}


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space --remove spaces from the input

parseString :: Parser LispVal
parseString = do
                char '"' --takes out " in string
                x <- many (noneOf "\"") --- takes out guts of string
                char '"' -- takes out closing " of string
                return $ String x -- returns just the guts

readExpr :: String -> String
--symbol is the user input
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do 
         args <- getArgs
         putStrLn (readExpr (args !! 0))


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool