{- |
Module : adventure.hs
Description : A (very much prototype!) adventure game architecture.

CS251 Final Project - MaryBethKery 5/2015
Usage: on command line type <game> to start the game (no <>)
-}

module Main (game) where

import AdventureParser
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


data Obj = Door {o_name :: String, o_desc :: String, to :: Room, from :: Room, lock :: Maybe Obj, open :: Bool}
		 | Lock {o_name :: String, o_desc :: String, locked :: Bool, key :: Obj, putn :: Obj -> String}
		 | Pickup {o_name :: String, o_desc :: String}

instance Entity Obj where
	describe ob = o_desc ob
	name ob = o_name ob


game = do 
          putStrLn $ spacer ++ "Welcome to the adventure!"
          putStrLn "You stand outside the Science Center. The time is 1:00 AM Saturday, and the last studiers have left. You forgot you notebook here and are back to find it."
          putStrLn "(hint: type <help> for commands)"
          repl --start repl

repl = do cmd <-getLine 
          putStrLn $ evalCommand $ parseExpr cmd
          repl


evalCommand ("help",xs) = spacer ++ helpString ++ spacer ++ tooManyWarning xs "help."

evalCommand ("look", xs) = (name room0) ++ ": " ++ (describe room0) ++ tooManyWarning xs "look."
evalCommand ("pocket", xs) = (tooManyWarning xs "pocket") ++ spacer ++ "In your coat pocket:  " ++ (foldl (\acc item -> acc ++ (name item) ++ ",") "" pocket) ++ spacer

evalCommand ("walk_to", [Nothing]) = "Where do you want to walk to?"
evalCommand ("walk_to", (Just x):xs') = case lookupObj x of
										(item:items') -> "walking to..." ++ name item
										_ -> "Where now? That's not a place here: " ++ x

evalCommand ("look_at", [Nothing]) = "What do you want to look at? Let's just look aimlessly.\n" ++ evalCommand ("look", [Nothing])
evalCommand ("look_at", (Just x):xs') = case lookupObj x of
										[] -> "No "++ x ++ " here to look at."
										(item:items') -> name item ++ ": " ++ describe item

evalCommand ("open", [Nothing]) = "What are you opening?"
evalCommand ("open", (Just x):xs') = case lookupObj x of
										(Door o_name o_desc to from lock open):xs' -> case lock of 
																					  Nothing -> "Door unlocked!"
																					  Just lk -> "Door locked. " ++ name lk ++ ":" ++ describe lk
										_ -> x ++ " isn't something around to open."

evalCommand ("putin", (Just x):(Just y):ps') = case (lookupObj x, lookupObj y) of
												(v:vs', t:ts') ->  putn t v
												_ -> "You want to put what in what now?"


evalCommand _ = "Sorry, I don't recognize that command."



helpString = "Actions: \n walk to __ \n look (generally around) \n look at __ \n open __ \n put __ in __ \n pocket \n help \n quit"
spacer = "\nxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"

tooManyWarning [Nothing] c = ""
tooManyWarning _ c = "\nSorry, didn't understand any of that past " ++ c



lookupObj n =  flt (items room0) ++ flt pocket
	where flt xs = filter (\x -> o_name x == n) xs



	{-lhelp n (items room0)
	where lhelp n xs = case xs of
					   [] -> Nothing
					   (x:xs') -> if (n== o_name x) then Just x else lhelp n xs'


	-}


onecard = Pickup {o_name = "onecard", o_desc = "Onecard ID with your name and picture."}
roomkey = Pickup {o_name = "room keys", o_desc = "Key to your room."}
pocket = [onecard, roomkey]

door0 = Door { o_name = "front door"
			 , o_desc = "You see the glass rotating doors to the Science Center."
			 , to = room1
			 , from = room0
			 , lock = Just door0lock
			 , open = False
			 }

door0lock = Lock { o_name = "swipe-access"
				 , o_desc = "The Science center is on onecard access nights and weekends."
				 , locked = True
				 , key = onecard
				 , putn = \x -> swipeDoor x
				 }

swipeDoor :: Obj -> String
swipeDoor itm = if name itm == name (key door0lock) then "The light turned green!" else "Didn't work."

room0 = Room { r_name = "Science Center outside"
			 , desc = "The steps are lit towards the <" ++ name door0 ++ "> ahead. Behind you in the darkness rain is falling. A raven caws overhead in the rafters."
			 , items = [door0, door0lock]
			 }

room1 = Room { r_name = "main atrium"
			 , desc = "affasf"
			 , items = []
			 }