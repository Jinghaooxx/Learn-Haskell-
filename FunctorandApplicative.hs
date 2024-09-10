-- ETA Conversion 
-- ETA conversion is a process of simplifying functions by removing redundant parameters 
-- f x ≡ g x   =>  f ≡ g
-- f x = g x  =>  f = g
-- f x = 1 x => f = (1 +)
-- f x y = sqrt (x + y)
-- f x = sqrt . (x +)  -- Partially applying `+`, `y` is removed using eta conversion


-- Most of the case, eta conversion works by identifying the last argument that is passed to a function and used in a simple way 


-- Operator Sectioning 
-- Haskell allows you to treat operators (like +, *, etc.) as regular functions. 
--You can "partially apply" these operators by filling in one side and leaving the other side to be used later.

-- In simple terms: You can turn + into a function and fill in one of the numbers 
f1 x = 1 + x        -- Adding 1 to x
f2 = (1 +)          -- You can remove `x` and write it as a function that adds 1


-- Function composition 
-- In haskell, you can chain multiple function together, where the output of one function is automatically passed as the input to the next 
-- In simple terms: You can combine functions like building blocks. The result of one goes into the next 

f3 x = sqrt (1 / x)     -- First divide 1 by x, then take the square root
f4 = sqrt . (1 /)       -- You can combine these two steps into one function using `.`

-- So the . operator is used for function composition. Function composition allow you to combine two or more functions into a single funciton 


-- In conclusion, Function composition and operator sectioning is one of the examples of eta conversion 

-- Point free style 
-- A point free style is a way of defining functions without mentioning their arguments explicitly
-- The goal is to write functions in terms of composition and other higher order function, removing unnecessary variables (or points)
-- this often leads to more concise and declarative code 

-- Example of Point Free style 
-- Original Function 
lessThan :: (Ord a) => a -> [a] -> [a]
lessThan n aList = filter (<n) aList

-- this is a function that takes in 2 argument, n and aList and it return a list that is lesser than n 

-- Apply ETA conversion 
lessThanEta :: (Ord a) => a -> [a] -> [a]
lessThanEta n = filter (<n)
-- But we still have n as argument. To go further and make this point free, we need to refactor the code more 

--Applying Operator Sectioning 
-- The expression (<n) applies n as the second argument to the < operator, which is inconvenient for further eta reduction.
-- We can instead express this as n >, which is equivalent 
lessThanEta1 :: (Ord a) => a -> [a] -> [a]
lessThanEta1 n = filter (n>)
-- the reason why we change from < to > is because of the position 
-- < n : when we do this we are like checking if something is less than n, so its like for each element x in aList check if it is < n 
-- and n in this case will be the second argument which make it hard to reduce 

-- So we change from < to > to ensure that n it is the first argument, since we know that <n and n> is equivalent 
-- After changing to n> we could use function composition to reduce the n 

lessThanPointFree :: (Ord a) => a -> [a] -> [a]
lessThanPointFree = filter . (>)
-- So in this case we use . to combine 2 particular function into a single function
-- so first it will check > then if it is greater than then it will filter out 
-- procedures
-- 1. n > 
-- 2. filter 


-- Exercise 
g :: Num a => a -> a -> a
g x y = x^2 + y

--ETA Conversion
g1 x = (x^2) (+)

g2 x = (x (^2)) (+)

g3 x = ((+) . (^2)) x

g4 = (+) . (^2)

-- 2. z a b c = (a+b)*c

--ETA Conversion 
z a b c = (a+b) (* c)
z1 a b = (a+b) (*)
z2  a b = (*) (((+) a) b)
z3 a = ((*) . ((+) a))
z4 a = (*) . ((+) a)
z5 a = ((*) .) ((+) a)
z6 a = (((*) .) . (+)) a
z7 = ((*) . ) . (+)
