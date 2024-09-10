import Prelude hiding (Nothing, Just, Maybe)
data Maybe a = Just a | Nothing
  deriving (Show)

-- | Justs are equal if both values are equal
-- | Nothings are equal
--
-- see https://tgdwyer.github.io/haskell2/#typeclasses
--
-- >>> Just 2 == Just 2
-- True
--
-- >>> Just 1 == Just 3
-- False
--
-- >>> Nothing == Nothing
-- True
--
-- >>> Nothing == Just 1
-- False
instance Eq a => Eq (Maybe a) where
  (==) :: Maybe a -> Maybe a -> Bool
  (==) (Just a) (Just b) = a == b 
  (==) Nothing Nothing   = True
  (==) _ _               = False