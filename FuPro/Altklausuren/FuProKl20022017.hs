{-# LANGUAGE LambdaCase #-}
module FuProKl20022017 where
import Data.Maybe
import Data.Array
import DynProg

--A1

a11 :: [Integer] -> [(Integer,Integer)]
a11 = map (\x -> (x,x)) . filter (<2)

a12 :: [(Int -> Bool)] -> Bool
a12 []      = True
a12 (x:xs)  = x 1 && a12 xs

-- (x,y):z:xs 
-- (1,2):z:xs
-- (1,2):(3,4):xs
-- (1,2):(3,4):[(5,6)]
-- [(1,2),(3,4),(5,6)]
--
-- (a:b:c):x:_
-- ('u' : 'v' : c) : 'x' : "y"
-- damit nicht moeglich
--
-- (x,y):a:as
-- ((\x -> x+1), 2):a:as
-- [((\x -> x+1), 2),(id, 3), ((+1), 5)]

a13 = foldl (\x y -> if y > 1 then x+y else 1) 0 [0,1,2,3]
--foldl(...) 1 [1,2,3]
--foldl(...) 1 [2,3]
--foldl(...) 3 [3]
--foldl(...) 6 []
--6

a14 = (\f -> f.f) (\x -> (\y -> x+y+1)3)2
--(\f ..) (\x -> (x+4))
-- (\x -> (x+4)) . (\x -> (x+4) )2
-- 10

--A2

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' trg (a:as)
    | a == trg  = True
    | otherwise = elem' trg as

filter' :: Eq a => (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (a:as)
    | (f a)     = a : filter' f as
    | otherwise = filter' f as

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _  = True
isPrefixOf' _  [] = False
isPrefixOf' (a:as) (b:bs)
    | a == b = isPrefixOf' as bs
    | otherwise = False

--A3

data Tree a = V a [Tree a]

--1.

rev :: Tree a -> Tree a
rev = \case
    (V a []) -> (V a [])
    (V a ts) -> do
        let tmpTs = map rev ts
        let newTs = reverse tmpTs
        (V a newTs)

--2.

treeAnd :: Tree Bool -> Bool
treeAnd = and . allTrees
    where
    allTrees = \case
        (V a []) -> [a]
        (V a ts) -> [a] ++ map treeAnd ts

--3.

row :: Int -> Tree a -> [a]
row n (V a ts)
    |n < 0      = []
    |n == 0     = [a]
    |otherwise  = case ts of
        [] -> [] 
        t  -> concat (map (row (n-1)) t)

--A4

class Monoid' m where
    mzero :: m
    mappend :: m -> m -> m

class Monoid' g => Group' g where
    invert :: g -> g

instance Monoid' Int where
    mzero   = 0
    mappend = (+)

instance Group' Int where
    invert i = i * (-1) 

--A5

data Exp   = Con Int 
            | Sum Exp Exp
            | Division Exp Exp

evalM :: Exp -> Maybe Int
evalM = \case
    (Con i)          -> return i
    (Sum e1 e2)      -> do
        exp1 <- evalM e1
        exp2 <- evalM e2
        return (exp1 * exp2)
    (Division e1 e2) -> do
        exp1 <- evalM e1
        exp2 <- evalM e2
        case exp2 == 0 of
            True  -> Nothing
            False -> return (exp2 `div` exp1)
--A6

xs = do
    x <- [1..10]
    y <- [1..20]
    if y>x then return (x*y) else []

xs' = 
    [1..10] >>= \x ->
    [1..20] >>= \y ->
    if y>x then return (x*y) else []

--A7

type Stack a = Trans [Int] a

push :: Int -> Stack ()
push i = T $ \x -> ((),i:x)

pop :: Stack Int 
pop = T $ \(x:xs) -> (x,xs)

addW :: Stack ()
addW = T $ \(x:y:xs) -> ((),((x+y):xs))

mul :: Stack ()
mul = T $ \(x:y:xs) -> ((),((x*y):xs))

prog :: Stack Int 
prog = T $ \x -> (((2+3*2) + 5),x)

--A8 

fDyn n = arr ! n
    where
    arr = mkArray f (0,10000000)
    f 0 = 1
    f 1 = 2
    f 2 = 3
    f n = arr ! (n-1) * arr ! (n-2) `div` arr ! (n-3)
