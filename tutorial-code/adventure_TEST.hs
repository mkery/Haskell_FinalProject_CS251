
module Main where

data Entity = Entity {name :: String, desc :: String}

data (Entity obj) => Room obj = Room {items :: [Entity]} derives Entity

data Door = Door {door :: Entity, to :: Room, from :: Room, locked :: Bool, open :: Bool}


main = do 
          putStrLn "Welcome to the adventure!"
          putStrLn "You stand outside the Science Center. The time is 1:00 AM Saturday, and the last studiers have gone home. You forgot you notebook here and are back to find it."
          putStrLn "(hint: type <help> for commands)"
          repl --start repl

repl = do cmd <-getLine 
          evalCommand cmd
          repl

evalCommand "help" = do putStrLn helpString
evalCommand "look" = do putStrLn ((name $ room room0) ++ ": " ++ (desc $ room room0))


helpString = "Actions: \n walk to __ \n look \n open __ \n put __ in __ \n pocket \n help"


door0 = Door { door = Entity {name = "front door", desc = "asdf"}
			 , to = room1
			 , from = room0
			 , locked = True
			 , open = False
			 }


room0 = Room { items = [door0]
			 }
			 Entity {name = "Science Center outside", desc = "The steps are lit towards the <" ++ name door0 ++ "> ahead. Behind you in the darkness rain is falling. A raven caws overhead in the rafters."}
			 

room1 = Room { room = Entity{name = "main atrium", desc = "affasf"}
			 , items = [] 
			 }