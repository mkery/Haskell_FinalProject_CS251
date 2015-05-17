
module Main (game) where

import ParseAdventure
import System.Exit (exitSuccess)

class Entity a where 
	describe :: a -> String
	describe a = "????"
	name :: a -> String
	name a = "????"

data Room = Room {r_name :: String, desc :: String, items :: [Obj]}
instance Entity Room where
 	describe r = desc r
 	name r = r_name r


data Obj = Door {d_name :: String, to :: Room, from :: Room, locked :: Bool, open :: Bool}
		 | Pickup {d_name :: String}

instance Entity Obj where
	describe d = ""
	name d = d_name d



game = do 
          putStrLn $ spacer ++ "Welcome to the adventure!"
          putStrLn "You stand outside the Science Center. The time is 1:00 AM Saturday, and the last studiers have left. You forgot you notebook here and are back to find it."
          putStrLn "(hint: type <help> for commands)"
          repl --start repl

repl = do cmd <-getLine 
          expr <- parseExpr cmd
          putStrLn expr
          repl

evalCommand "help" = do putStrLn $ spacer ++ helpString ++ spacer
evalCommand "look" = do putStrLn ((name room0) ++ ": " ++ (describe room0))
evalCommand "pocket" = do putStrLn $ spacer ++ "In your coat pocket:  " ++ (foldl (\acc item -> acc ++ (name item) ++ ",") "" pocket) ++ spacer
evalCommand "quit" = do exitSuccess
evalCommand "walk to " ++ x = do putStrLn "walking!"







helpString = "Actions: \n walk to __ \n look \n open __ \n put __ in __ \n pocket \n help \n quit"
spacer = "\nxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"

pocket = [Pickup {d_name = "onecard"}, Pickup {d_name = "room keys"}]

door0 = Door { d_name = "front door"
			 , to = room1
			 , from = room0
			 , locked = True
			 , open = False
			 }

room0 = Room { r_name = "Science Center outside"
			 , desc = "The steps are lit towards the <" ++ name door0 ++ "> ahead. Behind you in the darkness rain is falling. A raven caws overhead in the rafters."
			 , items = [door0]
			 }

room1 = Room { r_name = "main atrium"
			 , desc = "affasf"
			 , items = []
			 }