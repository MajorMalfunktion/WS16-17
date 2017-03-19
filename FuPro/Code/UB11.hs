module UB11 where
import Vlfun
import Data.Maybe

--A11.2

-- the main idea is to check whether a node <- nodes 
-- is an element of succ' in (closureF graph),
-- which returns a Graph with a function that has an equal subset to a prior function
-- therefore it determins whether it's cyclic
isCyclic :: Eq a => Graph a -> Bool
isCyclic graph = or $ map self nodes
        where
        -- generates closure = succ' as in closureF and uses closure in the self function
        G nodes closure = closureF graph
        self node       = elem node (closure node)

undirected :: Eq a => Graph a -> Graph a
undirected (G nodes sucs)
        = G nodes sucs'
        where
        sucs' x = union (sucs x) [x' | x' <- nodes, elem x (sucs x')]

--A11.3

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
        = case y of 
            0 -> Nothing
            n -> Just $ div x n

safeSqrt :: Int -> Maybe Int
safeSqrt x
        | x < 0     = Nothing
        | otherwise = Just $ intSqrt x

-- Hilsfunktion
intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral
--

f :: Int -> Int -> Int -> Maybe Int
--f x y z = saveDiv (fromJust $ safeSqrt x) (fromJust $ safeSqrt (fromJust $ saveDiv y z))
-- -safer- version:
f x y z = case (or [isNothing f1, isNothing f2]) of
            True    -> Nothing
            False   -> safeDiv (fromJust f1) (fromJust f2)
        where 
        f1 = safeSqrt x
        f2 = maybe Nothing safeSqrt $ safeDiv y z
