import qualified GHC.Base

-- Functor 
-- a functor is a type class that represent data structures that can be mapped over. 
-- it generalizes the idea of applying a function to the element inside the structure without changing the structure itself 

-- Functor Type Class 
class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

-- f is a type constructor that takes one type argument (e.g., [] for lists, Maybe, etc.).
-- fmap is the main function of the Functor type class. It takes:
-- A function (a -> b) that transforms values of type a into values of type b,
-- A functor f a, which is some data structure containing values of type a,
-- And returns a new functor f b, which is the same structure but now containing values of type b.
--The key is that fmap applies a function to each element inside the functor without changing the structure itself.

-- Implementation of Map 
myMap :: (t -> a) -> [t] -> [a]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- In haskell we can do like 
map1 :: [Integer]
map1 = map (\i->i+1) [1,2,3]
-- After applying eta reducton 
map2 :: [Integer]
map2 = map (+1) [1,2,3,4] 



safeMod :: Integral a => a-> a-> Maybe a
safeMod _ 0 = Nothing
safeMod numerator divisor = Just $ mod numerator divisor 

--Example with tree 
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
  deriving (Show)


tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

--Visualization 
-- Node 4
--  ├──Node 2
--  |   ├──Leaf 1
--  |   └──Leaf 3
--  └──Node 6
--      ├──Leaf 5
--      └──Leaf 7

instance Functor Tree where
   fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ Empty = Empty
   fmap f (Leaf v) = Leaf $ f v
   fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)


-- liftA2 
-- The Applicative type class also provides a function called liftA2, which is a shortcut for applying binary functions over two Applicative values:
lift = GHC.Base.liftA2 (+) (Just 3) (Just 2)


-- Practice questions 
-- Basic fmap
-- Write out the results of these expressions using fmap:

-- a. fmap (+1) (Just 5)
-- b. fmap (*2) [1, 2, 3]
-- c. fmap (not) (Just True)
-- d. fmap reverse (Just "hello")
-- e. fmap (+1) Nothing

a :: Functor f => (t -> a -> b) -> t -> f a -> f b
a f x = fmap (f x)

b :: Functor f => (t -> a -> b) -> t -> f a -> f b
b f x = fmap (f x)

c :: Functor f => (t -> a -> b) -> t -> f a -> f b
c f x = fmap (f x)

d :: Functor f => (t -> a -> b) -> t -> f a -> f b
d f x = fmap (f x )


--  Functors with Lists
-- Given the following lists, apply a function to each element using fmap:

-- a. Increment each number in [1, 2, 3].
-- b. Negate each number in [-5, 0, 5].
-- c. Square each number in [1, 2, 3, 4].

la f x = f <$> x

lb f x = f <$> x

lc f x = f <$> x

-- 3. Functor with Custom Types
-- Define a custom data type Box that holds a value of any type. Then, make Box an instance of Functor and implement fmap for it. Here's the type definition:

data Box a = Box a deriving (Show)

-- Implement fmap for Box.
-- Apply fmap to a Box Int and a Box String.

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box x) = Box (f x)

-- -- Functors with Nested Contexts
-- Write the result of fmap applied to functions over nested functors:

-- a. fmap (fmap (+2)) (Just [1, 2, 3])
-- b. fmap (fmap (not)) [Just True, Nothing, Just False]
-- c. fmap (fmap (*3)) (Just (Just 4))

na f x = fmap (fmap f ) x

nb f x = fmap (fmap f ) x 

nc f x = fmap (fmap f ) x 