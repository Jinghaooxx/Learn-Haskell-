-- An empty list 
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
emptylist :: [a]
emptylist = []

-- A list of integer 
nums :: [Integer]
nums = [1,2,3,4]

-- Access the head of the list (first element)
firstElement :: Integer
firstElement = head nums 

-- Access the tail of the list (all the element except the first)
rest :: [Integer]
rest = tail nums

-- Concatenate two list 
concatenate :: [Integer]
concatenate = nums ++ [6,5]

--Every num in the list multiply by 2 
mulitply :: [Integer]
mulitply = map (*2) nums 

-- Range operator 
range :: [Integer]
range = [1..4]

-- add element into the list 
-- front of the list 
addFront :: [Integer]
addFront = 1:[2,3,4]

-- Chain elements
chain :: [Integer]
chain = 1:2:3:[4]


-- Tuples 
-- A tuple of an integer and a string
myTuple :: (Integer, String)
myTuple = (1,"hello")

-- Access the first element of the tuple 
first :: Integer
first = fst myTuple

-- Access the second element of the tuple 
second :: String
second = snd myTuple


-- You can use `:` to pattern match lists in function definitions.
-- Note the enclosing `()` to delimit the pattern for the parameter.

-- length [] = 0
-- length (x:xs) = 1 + length xs -- x is bound to the head of the list and xs the tail
-- x is the first element of the list 
-- xs is the tail of the list 

-- -- (although you don’t need to define `length`, it’s already loaded by the prelude)


-- List 
--sum 
sum1 :: Integer
sum1 = sum [1,2,3] -- sum up all the number in the list 

-- minimum, find the minimum value in the list 
min1 :: Integer
min1 = minimum [1,2,3,4,5,6,0] 

-- maximum, find the largest value in the list 
max1 :: Integer
max1 = maximum [1,2,3,5,9,10,20]

-- map 
f :: Integer -> Integer
f = (+1)

addone :: [Integer]
addone = map f [1,2,3,4,5,6]

-- Destructure 

t :: (Integer, String)
t = (1,"hello") -- define variable t to a tuple of an Int and a String.b :: b

b :: String
a :: Integer
(a,b) = t


-- Lazy by default 
-- Lambda Calculus
-- λx. x

-- JavaScript
-- x => x

-- Haskell
-- \x -> x

-- Y combinator 
-- y :: (t -> t) -> t
-- y = \f -> (\x -> f (x x)) (\x -> f (x x))

--Quick sort list:
-- Take head of list as pivot
-- Take tail of list as rest 
-- return 
-- QuickSort( elements of rest < pivot ) ++ (pivot : QuickSort( elements of rest >= pivot ))

sort [] = []
sort (pivot:rest) = lesser ++ [pivot] ++ greater
    where 
        lesser = sort $ filter (<pivot) rest 
        greater  = sort $ filter (>=pivot) rest 


-- Pattern matching 
-- Pattern matching in Haskell is a way of checking and breaking down the shape of data. 
--You tell Haskell to look at a value and see if it fits a certain pattern. If it does, Haskell will do something based on that pattern.
-- It's like saying: "If the data looks like this, do this. If it looks like that, do that."

k :: [a] -> Int
k [] = 0                -- Case 1: if the list is empty, return 0
k (x:xs) = 1 + k xs -- Case 2: if the list has elements, count 1 for the head and call length on the rest

-- Another example of pattern matching 
describeList :: [[a]] -> String
describeList [] = "Empty List"
describeList [x] = "List with one element"
describeList [x:xs] = "List with more than one element"

-- if else 
-- if <condition> then <case 1> else <case2>
fibs :: (Eq t, Num t, Num a) => t -> a
fibs n = if n == 0 then 1 else if n == 1 then 1 else fibs (n-1) + fibs (n-2)

-- Guards 
fib n 
    | n == 0 = 1 
    | n == 1 = 1 
    | otherwise = fib(n-1) + fib(n-2)

-- Basically like if else, if n is 0 then 1 if n = 1 then 1 else recursion 

--case 
-- case <expression> of 
-- <pattern1> -> <result if pattern1 matches>
-- <pattern2> -> <result if pattern2 matches>
-- _ -> <result if no pattern above matches>

fibs1 :: (Eq t, Num t, Num a) => t -> a
fibs1 n = case n of
  0 -> 1
  1 -> 1
  _ -> fibs1 (n-1) + fibs1 (n-2)