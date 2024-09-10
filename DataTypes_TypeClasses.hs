-- Algebraic Data Types (ADTs)
-- we can declare custom types for data in haskell using the data keyword.
data ConsList = Nil | Cons Int ConsList

-- the | operator represents as the union types 
-- Nil is Null 

l :: ConsList
l = Cons 1 $ Cons 2 $ Cons 3 Nil

-- Aside: data vs newType 
-- We can construct a type UserId with one parameter, Int 
data UserId = UserId Int -- data could take in multiple data type 
newtype UserId1 = UserId1 Int -- new type can only take in one type 

-- Pattern matching 
consLength Nil = 0
consLength (Cons _ rest) = 1 + consLength rest

-- Record syntax 
-- Define the Student data type
data Student = Student {id:: String , name:: String, mark::Int}

-- Define the best function
best :: [Student] -> Student -> Student
best [] b = b
best (a@(Student _ _ am):rest) b@(Student _ _ bm) =
  if am > bm
  then best rest a
  else best rest b

-- Create some students
alice :: Student
alice = Student "Alice" "A" 90
bob :: Student
bob = Student "Bob" "B" 85
charlie :: Student
charlie = Student "Charlie" "C" 95


-- @It binds a name to the whole value while simultaneously allowing you to pattern match on parts of that value.
-- It’s a way of saying, "I want to use both the whole thing and its parts
-- patternName@pattern



-- Type Classes 
-- Type classes in haskell are a way to define generic behaviour that can be applied to different types 
-- A type classes defines a set of functions (or operations) that can applied to different types. if a type belong to a type class, it must provide 
    -- implmentation for those function, this is a process called instance declaration 

-- Example: The EQ Type Class 
-- class Eq a where 
--     (==) :: a -> a -> Bool -- Take in two variable: In ts x => y => f 
--     (=/) :: a -> a -> Bool 

-- Eq is a type class 
-- a is a type that can belong to this class 
-- the class defines two functions: == (equals) and (=/) not equals

-- Defining an instance 
data Color = Red | Green | Blue deriving (Eq, Ord, Show)


-- Equality (==  =/)
main = do
    print (Red Prelude.== Green)  -- False
    print (Blue Prelude.== Blue)  -- True

-- Comparison (> < <= >=)
main1 = do
    print (Red < Green)  -- True
    print (Blue > Red)   -- True


-- print statement 
main2 = do
    print Red    -- "Red"
    print Green  -- "Green"

instance Num Color where
    Red + Red = Red
    Red + Green = Green
    Red + Blue = Blue
    Green + Green = Blue
    Green + Blue = Red
    Blue + Blue = Green
    fromInteger 0 = Red
    fromInteger 1 = Green
    fromInteger 2 = Blue
    fromInteger _ = error "No corresponding color"
    -- Other required functions can be implemented similarly

main3 = do
    print (Red + Green)    -- Green
    print (Green + Blue)   -- Red
    print (Blue + Blue)    -- Green


--Creating custom instances of type classes
data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Ord, Enum)

-- Deriving is a features that allows you to automatically generat instance of certain type classes for your custom data types. 
-- instead of manually writing out instance definition Haskell can infer the standard behavior for some common type classes, such as Eq, Ord, Show, and others.
-- When you use deriving, Haskell automatically creates an instance of a type class for your data type with default implementations of the type class's functions. 

main4 = do
    print (Spade < Club)
    print (Spade > Heart)
    print (Spade == Heart)
    print (show Heart)
    print [Spade .. Heart]


instance Show Suit where
    show Spade   = "^"   -- Custom symbol for Spade
    show Club    = "&"   -- Custom symbol for Club
    show Diamond = "O"   -- Custom symbol for Diamond
    show Heart   = "V"   -- Custom symbol for Heart

main5 = do
    print [Spade .. Heart]
    print (show Spade)


-- Maybe 
--Maybe is a built-in algebraic data type in Haskell that represents an optional value.
-- It is commonly used when an operation may fail or return no meaningful result. 

data Maybe a = Nothing | Just a
-- Nothing represent absence of value 
-- Just a represent a value of type a 
-- Just = is one of the constructors for the Maybe type. it is used to indicate that an operation has sucessfully return a value 

-- Why is Maybe useful 
--The Maybe type is a safe way to handle operations that may not return a result. 
--Instead of returning an error or crashing the program, it allows you to explicitly handle the case where something might not be available (i.e., Nothing), 
--or where a valid result is returned (i.e., Just a).

phonebook :: [(String, String)]
phonebook = [("Bob", "01788 665242"), ("Fred", "01624 556442"), ("Alice", "01889 985333")]

