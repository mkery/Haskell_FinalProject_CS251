import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

{-
following tutorial at http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
to figure out how to make a simple parser for my adventure game
-}

simple :: Parser ()
simple = letter

parseWalk :: Parser String
parseWalk = do
                symbol "walk to " --takes out " in string
                x <- many (noneOf "\n") --- takes out guts of string
                return $ x -- returns just the guts

readExpr :: String -> String
--symbol is the user input
readExpr input = case parse parseWalk input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do 
         args <- getArgs
         putStrLn (readExpr (args !! 0))

