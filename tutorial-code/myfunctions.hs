
--intro toy programs
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber' x = (if x > 10 then x else x*2) + 1

{-|
--Shape example for Type constructors
data Shape = Circle Float Float Float | Rectange Float Float Float Float deriving (Show)

--good style to declare types, though it can be implicitly inferred
surface :: Shape -> Float 
surface (Circle _ _ r) = pi * r ^ 2
--The $ operator is for avoiding parenthesis. abs(x2 - x1)
surface (Rectange x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  
-}

--data type and value constructor can have the same name Point/Point
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)