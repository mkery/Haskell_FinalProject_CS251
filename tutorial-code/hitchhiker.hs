{-

Following along with exercises and commenting code examples in 
https://wiki.haskell.org/Hitchhikers_guide_to_Haskell

-}



{-module Main where
main = do let x = getContents --delayed evaluation, action not run yet
		  -- 300 lines of code here
		  input <- x
		  putStrLn ("DEBUG: got input " ++ input)
		  --compute sol and print-}

{-module Main where
	process = do putStrLn "gonna run c"
			  c
			  putStrLn "done with c!"

	c = do a <- getContents
		   print a
		   putStrLn "cccc"
		   -}



{-- Taken from 'exercise-1-1.hs'
module Main where
c = putStrLn "C!"
 
 --function combine taking fun args before and after
combine before after =
  do before
     putStrLn "In the middle, stop for lemonade"
     after
 
main = do combine c c
          let b = combine (putStrLn "Hello!") (putStrLn "Bye!")
          let d = combine (b) (combine c c)
          putStrLn "So long!"
-}

--exercise todo: write a name with a greeting
{-}
module Main where

greeting1 = do 
	putStrLn "Hello, who might you be?"
	name <- getLine
	putStrLn ("Okay then, " ++ name)
	return name

greeting2 = do
	name <- greeting1
	putStrLn ("So, " ++ name ++ ", what's your favorite color?")
	color <- getLine
	putStrLn ("Err... interesting choice, " ++ color ++ ".")
-}


-- Taken from 'cd-fit-2-1.hs'
import Text.ParserCombinators.Parsec
 
-- parseInput parses output of "du -sb", which consists of many lines,
-- each of which describes single directory
parseInput = --do is marking monadic actions/values
  do dirs <- many dirAndSize
     eof :: Parser ()
     return dirs
 
-- Datatype Dir holds information about single directory - its size and name
data Dir = Dir Int String deriving Show
 
-- `dirAndSize` parses information about single directory, which is:
-- a size in bytes (number), some spaces, then directory name, which extends till newline
dirAndSize = 
  do size <- many1 digit -- takes in a digit, many #'s but 1 group?
     spaces -- something about spaces???
     -- take the directory name as multiple chars up until a newline
     -- `manyTill` is to mark function as infix
     dir_name <- anyChar `manyTill` newline 
     return (Dir (read size) dir_name)

