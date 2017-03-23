module DynProg where
import Data.Array
--class Ord a => Ix a where
--    range :: (a,a) -> [a]
--    index :: (a,a) -> a -> Int
--    inRange :: (a,a) -> a -> Bool
--    array :: Ix a => (a,a) -> [(a,b)] -> Array a b
--    (!) :: Ix a => Array a b -> a -> b
--    (//) :: Ix a => Array a b -> [(a,b)] -> Array a b
--    bounds :: Ix a => Array a b -> (a,a)
--
--    rangeSize :: (a,a) -> Int
--    rangeSize (a,b) = index (a, b) b+1

mkArray :: Ix a => (a -> b) -> (a,a) -> Array a b
mkArray f bounds = array bounds [(x,f x) | x <- range bounds]
