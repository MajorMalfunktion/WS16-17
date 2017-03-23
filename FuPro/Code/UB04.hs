module UB04 where

--a4.1

a41, b41, c41 :: Int -> Int
a41 n
    |even n     = n `div` 2
    |otherwise  = 3 * n + 1

b41 n
    | n < 10    = 2 * n
    | otherwise = n + 30

c41 n
    |even n     = n + 10 
    |otherwise  = c41 $ n + 3

--a4.2

flatten :: [a] -> [a]
flatten []      = []
flatten (a:as)  = a ++ flatten as

toTupel :: [a] -> [b] -> [(a,b)]
toTupel []     _      = []
toTupel _      []     = []
toTupel (a:as) (b:bs) = (a,b) : toTupel as bs

takeUpTo :: Num a => a -> [a] -> a
takeUpTo _ []     = []
takeUpTo n (a:as) 
    | n == a    = []
    | otherwise = a : takeUpTo n as

--a4.3

power :: Num a => a -> Int -> a
power base 0    = 1
power base expo = base * power base (expo - 1)

summe :: Num a ->  [a] -> a
summe []     = 0
summe (a:as) = a + summe as

--a4.4
--
--take 2 [2,3,..]
--[2,3]
--
--head[3,..]
--3
