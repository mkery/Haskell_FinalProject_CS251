{- 
MaryBethKery
Final Project toy programs
Earlier pset problems, originally given for Racket or ML, done in
Haskell: to practice in Haskell and see the comparisons at the same
time
-}

-- Lists.1.a Write a function merge that merges two lists into one as in merge-sort
merge2 :: (Ord a) => [a] -> [a] -> [a] 
-- (Ord a) contraints type a to things that can be ordered, aready cooler than the racket version :)
merge2 [] ys = ys
merge2 xs [] = xs
merge2 (x:xs') (y:ys') = if x<y then x:(merge2 xs' (y:ys')) else y:(merge2 (x:xs') ys')

-- ALT with more pattern matching
merge2_alt :: (Ord a) => [a] -> [a] -> [a] 
-- (Ord a) contraints type a to things that can be ordered, aready cooler than the racket version :)
merge2_alt [] ys = ys
merge2_alt xs [] = xs
merge2_alt (x:xs') (y:ys') 
	| x<y = x:(merge2_alt xs' (y:ys')) 
	| otherwise = y:(merge2_alt (x:xs') ys') 



-- Lists.1.b Write a function rev that takes a list xs and reverses its order
rev :: [a] -> [a]
rev xs =
	let 
		revh :: [a] -> [a] -> [a] --defining a helper for tail recursion
		revh [] acc = acc
		revh (x:xs') acc = revh xs' (x:acc) 
	in  revh xs []
		
-- ALT with foldl
rev_fd :: [a] -> [a]
rev_fd xs = foldl (\acc x -> x:acc) [] xs 

-- Lists.1.c Write a function deep-rev that takes any argument x and deeply reverses it

-- Lists.1.d Write a function contains-multiple that takes an integer m and a list of integers ns that returns #t if m evenly divides at least one element of the integer list ns; otherwise it returns #f.
contains_mult :: Int -> [Int] -> Bool
contains_mult n [] = True
contains_mult n (x:xs')
	| (x `mod` n == 0) = contains_mult n xs'
	| otherwise = False

-- ALT with hof
contains_mult_alt :: Int -> [Int] -> Bool
contains_mult_alt n xs = foldl (\acc x -> if x `mod` n == 0 then True else False) True xs 

-- HOF.1.a Write a function lookup that takes a key k and an association list
lookup_t :: (Eq a) => a -> [(a,b)] -> Maybe b -- using options!
lookup_t n [] = Nothing
lookup_t n ((x, v):xs')
	| (n == x) = Just v
	| otherwise = lookup_t n xs'
-- There's also a built-in lookup

-- Art.2.a Write the function zip : ('a list * 'b list) -> ('a * 'b) list to compute the product of two lists of arbitrary length
zip_t :: [a] -> [b] -> [(a,b)]
zip_t as bs = 
	let 
		ziph (a:as') (b:bs') acc = ziph as' bs' ((a,b):acc)
		ziph _ _ acc = acc
	in ziph as bs []
-- also built in zip function in Prelude

-- Art 2.b Write the inverse function, unzip : ('a * 'b) list -> ('a list * 'b list)
unzip_t :: [(a,b)] -> ([a],[b])
unzip_t xs = 
	let 
		unziph :: [(a,b)] -> ([a],[b]) -> ([a],[b])
		unziph ((a,b):xs') (as,bs) =  unziph xs' (a:as, b:bs)
		unziph [] fin = fin
	in unziph xs ([],[])

-- comments with dashes
zip_r :: [a] -> [b] -> [(a,b)]
zip_r [] ys = []
zip_r (x:xs') (y:ys')  = (x,y) : (zip_r xs' ys')

cats = 1 : cats

ones = [1..]