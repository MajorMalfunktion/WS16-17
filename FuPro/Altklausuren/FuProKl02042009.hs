module Klausur02042009 where
import Data.Array 
--  A1.1
--      foldr (\x (y,z) -> (y*z + x, z+x))(0,0) [1,1,2]
--  =   foldr (\x (y,z) -> (y*z + x, z+x))(2,2) [1,1]
--  =   foldr (\x (y,z) -> (y*z + x, z+x))(5,3) [1]
--  =   (16,4)

--  A1.2

--data IMP a  = P a
--            | P (IMP a) (IMP a)  <--- P nur ein a

--a TOP = P a (a TOP)  <--- Syntaxfehler
--      | Q a

data SINC a b   = F (SINC a b) a
                | R a b

--  A2

data Tree a = T a [Tree a]

breite :: Tree a -> Int
breite (T _ ts) =   maxBreite ts
                    where 
                    maxBreite [] = 1
                    maxBreite ts = max (length ts) 
                                    (maxBreite $ ts >>= children)
                        where
                        children (T _ ts) = ts

--  A3

primes :: [Int]
primes =    sieve [2..]
            where 
            sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

g :: Int -> [Int]
g n =   filter (isPrime) [a * b - 1 | a <- prim, b <- prim, a <= b]
        where 
        prim = kleinerglN primes n

kleinerglN :: [Int] -> Int -> [Int]
kleinerglN (x:xs) n |x <= n     = x : kleinerglN xs n
                    |otherwise  = []

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]
            where
            factors n = [x | x <- [1..n], n `mod` x == 0]

--  A4
mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bounds = array bounds [(x, f x) | x <- range bounds]

gFib :: Int -> Int -> Int
gFib k n    |k <= 0     = 0
            |n < 0      = 0
            |n < k      = n
            |otherwise  = sum [gFib k (n-i) | i <- [1..k]]

gFib2:: Int -> Int -> Int
gFib2 k n = arr ! n
            where
            arr = mkArray f (0,n)
                where
                f i |k <= 0     = 0
                    |n < 0      = 0
                    |n < k      = n
                    |otherwise  = sum [arr ! (i-var) | var <- [1..k]]

--  A5

zz :: Int -> IO Int 
zz n = return n

trial :: IO()
trial = do
        x <- zz 50
        trial2 x 1

trial2 :: Int -> Int -> IO()
trial2 x t =    do
                putStrLn "Raten Sie eine Zahl zwischen 1 und 50"
                nStr <- getLine
                let n = read nStr
                if (n == x) 
                    then putStrLn $ show t
                    else trial2 x (t+1)

--  A6.1


