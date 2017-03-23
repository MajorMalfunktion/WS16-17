{-# LANGUAGE LambdaCase#-}

module Klausur19022009 where
import Data.Array
import DynProg

-- A1
--1.
--
-- map :: (a -> b) -> [a] -> [b]
-- 
-- map (+1) [1,2]
-- 2 : map (+1) [2]
-- 2 : 3 : map (+1) []
-- 2 : 3 : []
-- [2,3]
--
--
-- filter :: (a -> Bool) -> [a] -> [a]
--
-- filter (>1) [1,2]
-- filter (>1) [2]
-- 2 : filter (>1) []
-- 2 : []
-- [2]
--
-- zipWith :: (a -> b) -> [a] -> [b]
--
-- zipwith (+) [1,2] [4,3]
-- 5 : zipwith (+) [2] [3]
-- 5 : 5 : zipwith (+) [] []
-- 5 : 5 : []
-- [5,5]

--2.
a22 = foldl f 1 [6,1,5,4,5]
    where
    f = (\u v -> if (v > u*u) && (u /= 0) then v `div` u else u - v) 

-- foldl f 6 [1,5,4,5]
-- foldl f 5 [5,4,5]
-- foldl f 0 [4,5]
-- foldl f -4 [5]
-- foldl f -9 []
-- -9

--3.
-- return :: a -> m a
-- (>>=)  :: m a -> (a -> m b) -> m b
-- (>>)   :: m a -> m b -> m b
-- fail   :: String -> m a
-- m >> m' = m >>= const m'


--A2

data Tree a = T a [Tree a]

isBalanced :: Tree a -> Bool
isBalanced (T a ls) = isBalanced2 (len ls) ls
    where
    ltrixen = \case []      -> 0 
                    (a:as)  -> 1 + len as
    len = \case []      -> 0
                (a:as)  -> 1 + len as
    diffOne n m
        | and [(n+1>m),(n-1<m)] = True
        | otherwise = False
    isBalanced2 _ [] = True
    isBalanced2 n (l:ls)
        = case diffOne n $ len [l:ls] of
            False -> False 
            True  -> isBalanced2 n ls 

--A3

g :: Integer -> [Integer]
g n = g2 $ map (* (toInteger 30)) [(toInteger 1)..n]
    where
    g2 [] = []
    g2 (a:as) 
        | has30 a   = a : g2 as
        | otherwise = g2 as
    has30 n 
        = (len([t | t 
                <- [1..(floor . sqrt $ fromInteger n)] , 
                n `mod` t == 0])) == 30
    len = \case []      -> 0
                (a:as)  -> 1 + len as

--A4
-- Signatur stimmt nicht
--fDyn :: Integer -> Integer
fDyn 
    = mkArray f (0,1000000)
    where
    f 0 = 1
    f 1 = 1
    f n | n < 0     = 0
        | otherwise = sum [fDyn!i |
                            i <- [(n `div` 3) .. (n `div` 2)]]

--A5

makeMatrixUpTo :: IO()
makeMatrixUpTo
    =   do
        putStrLn "Write a Number greater than 2 and smaler than 10"
        line <- getLine
        let num = read line
        if num > 2 && num < 10
            then printAll 1 num
            else putStrLn "Fehlerhafte Eingabe"
    where
    zeile i n = concat [show (i*j) ++ "\t"| j <- [1..n]]
    printAll n m
        =   do
            case n <= m of
                True -> do
                    let line = zeile n m
                    putStr line
                    putStr "\n"
                    printAll (n+1) m
                False -> putStrLn ""

-- A6


