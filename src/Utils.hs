module Utils where

import           Data.List
import           Data.Function
import           Control.Monad

-- | Nested map
--
-- >>> (<$$>) head [[[1],[2,3,4]],[[5,6],[7]]]
-- [[1,2],[5,7]]
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

infix 4 <$$>

groupEqual :: Eq b => (a -> b) -> [a] -> [[a]]
groupEqual = groupBy . ((==) `on`)

concatMapM :: (Monad f) => (a1 -> f [a2]) -> [a1] -> f [a2]
concatMapM f = (join <$>) . mapM f

-- | Update a value preserving order, if it does not exist, add it
--
-- >>> update 3 [1..5]
-- [1,2,3,4,5]
--
-- >>> update 10 [1..5]
-- [10,1,2,3,4,5]
update :: Eq a => a -> [a] -> [a]
update x l | x `elem` l = map' (x ==) (const x) l
           | otherwise  = x : l

-- | Map on a condition
map' :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
map' p f = map (\y -> if p y then f y else y)

-- | Keep values of first list and add values of second list which do not exist in first.
--
-- >>> combineLatest [1..3] [1..5]
-- [1,2,3,4,5]
--
-- >>> combineLatest [1..10] [1..5]
-- [1,2,3,4,5,6,7,8,9,10]
combineLatest :: Eq a => [a] -> [a] -> [a]
combineLatest l1 l2 = l2 ++ (l1 \\ l2)

-- | Left-biased choice on @Maybe@
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust _        y = y

minus :: Num a => a -> a -> a
minus = (-)

sortAlong :: Eq c => (b->c) -> [c] -> [b] -> [b]
sortAlong f order = map snd . sortOn fst . map (\x -> (lookup (f x) z, x))
    where
    z = zip order [0..]