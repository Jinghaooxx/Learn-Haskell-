-- Applicative 
-- In Haskell, Applicative is a type class that allows us to apply function inside "Computational contexts". A Computational context could be something like 
-- a Maybe, a list or IO action 

--Applicative is similar to Functor, which allows us to use the fmap function to apply a regular function to a value inside a context.
-- But Applicative goes a step further—it lets us apply functions that are already inside a context to values inside a context.

-- When working with Applicative, there are two important elements need to consider 
-- 1. pure: This takes a regular value and puts it inside the applicative context. For example using pure 3 with Maybe will give you Just 3. Using it with lists will give you [3].
-- 2.<*> (pronounced "apply"): This is the operator that takes a function inside a context and applies it to a value inside a context.
--   For instance, if you have Just (+3) and Just 2, <*> allows you to apply the function (+3) to the value 2 inside their Maybe contexts.

-- How does <*> works ?
-- for example: Just (+3) <*> Just 2
--You have a function +3 wrapped inside a Maybe context (Just (+3)).
-- You also have a value 2 inside a Maybe context (Just 2).
-- <*> takes the function from Just (+3) and applies it to the value inside Just 2.
-- <*> allows you to take a function inside a context (in this case, Just (+3)) and 
-- apply it to a value inside the same context (in this case, Just 2). The result is the function applied to the value, 
-- but still inside the context (Just 5).
-- so bascially it just combine 2 function tgt if they have the same context 

-- Example 2 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import qualified GHC.Base
import Control.Applicative
{-# HLINT ignore "Use <$>" #-}
list1 :: [Integer]
list1 = [(+1), (+2)] <*> [1,2,3,4]

--Output : [2,3,4,5,3,4,5,6]
-- Perform (+1) then (+2) then combine 

-- Example 3
-- pure: pure allows us to take a normal value or function and wrap it into an applicative context. 
-- Without pure, you wouldn't be able to turn a regular value like 3 into Just 3 in a Maybe context, or 3 into [3] in a list context.

pure1 :: Maybe Integer
pure1 = pure (+) <*> Just 3 <*> Just 2
--pure acts as a converter that takes a regular value and puts (or "lifts") it into the same context as the applicative you're working with. 
-- This "context" could be something like Maybe, lists ([]), or IO, and pure helps ensure that the value or function is in the right "form" so that it can work with other values in the same context.
-- pure 5 :: Maybe Int  -- Result: Just 5
-- pure 5 :: [Int]  -- Result: [5]
-- pure 5 :: IO Int  -- Result: an IO action that returns 5


-- Alternative way
pure2 :: Maybe Integer
pure2 = (+) <$> Just 3 <*> Just 2
pure3 :: [Integer]
pure3 = (+) <$> [1,2] <*> [1,2,3,4]


-- Using Applicative for Binary Constructors
-- In haskell, constructor like tuples or custom data types can also be considered functions. For example, the tuple constructor (,) is a function that takes 
-- two arguments and return a tuple 

-- When we use the Applicative operator <$> (which is just fmap) with the tuple constructor and two Maybe values, it creates a tuple of those two values
tuple :: Maybe (Integer, Integer)
tuple = (,) <$> Just 3 <*> Just 2
-- (,): Tuple constructor 
-- <$>: fmap 
-- Just 3 <*> Just 2: context of Maybe 
-- What this function do is like, it make Just 3 and Just 2 value as a tuple (3,2) and this is a Maybe context tuple 
-- Output: Just (3,2)

-- Using Applicative for Ternary constructors
-- now consider a custom data type Student 
data Student = Student { id::Integer, name::String, mark::Int } deriving Show
--The Student constructor is a ternary function that takes three arguments (an Integer, a String, and an Int) and creates a Student.
names :: [(Integer, String)]
names = [(1, "Alice"), (2, "Bob"), (3, "Charlie")]

marks :: [(Integer, Int)]
marks = [(1, 90), (2, 85), (3, 78)]

-- Example of how to create a student using the value inside the Maybe 
lookupStudent :: Integer -> Maybe Student
lookupStudent sid = Student sid <$> lookup sid names <*> lookup sid marks

-- Student sid: it will appear in a form of object 
-- <*>: combine name and marks 
-- <$>: fmap : find the studnet id that matches 


-- Creating a Deck of Cards Using Applicative
--In your card example, you have two data types: Suit and Rank,
-- which both derive Enum, meaning they can be automatically enumerated (e.g., Spade .. will generate all the suits).
data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Ord, Enum, Bounded, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show, Bounded)

data Card = Card Suit Rank
  deriving (Eq, Ord, Show)

cards :: [Card]
cards = Card <$> [Spade ..] <*> [Two ..]

-- Applicative Practice Questions
-- 5. Basic <*> Usage
-- Apply functions wrapped in a Maybe context to values also wrapped in a Maybe:

-- a. Just (+2) <*> Just 3
-- b. Just (*3) <*> Nothing
-- c. Nothing <*> Just 5
-- d. Just (++) <*> Just "Hello, " <*> Just "World!"

a f x = Just f <*> Just x

b f x = Just f <*> Just x

c f x = f <*> Just x 

d f x v = Just f <*> Just x <*> Just v  

-- 6. Lists as Applicatives
-- Use the <*> operator with lists to generate combinations:

-- a. [(+1), (*2)] <*> [1, 2, 3]
-- b. [(*2), (+3)] <*> [4, 5]
-- c. [(++ "!"), reverse] <*> ["hello", "world"]

laA :: Applicative f => f (a -> b) -> f a -> f b
laA f x  =  f <*> x 

laB :: Applicative f => f (a -> b) -> f a -> f b
laB f x = f <*> x   

laC :: Applicative f => f (a -> b) -> f a -> f b
laC f x = f <*> x 


-- 7. Using pure with Applicatives
-- For the following values, use pure to lift them into an applicative context and then combine them using <*>:

-- a. pure (+) <*> Just 5 <*> Just 6
-- b. pure (,) <*> Just 3 <*> Just "three"
-- c. pure (*) <*> [1, 2, 3] <*> [4, 5]
-- d. pure (++) <*> pure "Good" <*> pure " Morning"
seva f x c = f <*> Just x <*> Just c 


-- Alternative Typeclass 
-- The alternative typeclass is an extension of the applicative type class. while applicative allows you to work with computations that are structured in a context 
-- such as Maybe or lists
-- Alternative adds extra functionality, particularly useful for handling computations that can fail or offer multiple outcomes

-- it introduces two main concepts
-- 1. empty: Represent failure or no result 
-- 2. <|>: Represents a choice between two computations 

-- Key functions in Alternative 
-- 1. empty 
-- Definition: empty :: f a 
-- empty represents a computation that has no result or has failed. It's like a "default" failure value for types that are instances of Alternative.
-- For Maybe, empty is Nothing, meaning "no result."
-- For lists, empty is [], an empty list, meaning "no values."

-- 2. <|>
-- Definition: <|> :: f a -> f a -> f a
-- <|> is used to combine two computations of the same type. If the first computation succeeds, it returns its result. If the first one fails, it tries the second computation.
-- For Maybe, <|> chooses the first Just if it's present; otherwise, it picks the second.
-- For lists, <|> concatenates two lists, providing all possible values.

-- Example of Alternative 
-- Example 1: Maybe 
-- Consider the Maybe type, which represents computations that might succeed (Just value) or fail (Nothing).


alternative1 :: Maybe Integer
alternative1 = Just 2 <|> Just 5

alternative2 = Nothing <|> Just 5 
-- it basically have the same concept as the or gate, it will first try the first element if it success then it will return the first element, else it goes to the second element 
-- in the case of if both element fails it will return nothing 
-- e.g 
alternative3 = Nothing <|> Nothing

-- It acts differently when it comes to list 
alternativeList1  = [1,2] <|> [3,4]
-- if the first element success it will concatenate with the second element and return as one element 

-- Case where the first element fails 
alternativeList2 = [] <|> [1,2,3]
-- it will only return the second element 

-- Case where both condition fails 
alternativeList3 = [] <|> []
-- It will return an empty list 