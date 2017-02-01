module Vlfun where

data Tree a     = V a 
                | F a [Tree a]

data BintreeL a = Leaf a 
                | Bin a (BintreeL a)(BintreeL a)

data Bintree a  = Empty
                | Fork a (Bintree a) (Bintree a)
                deriving(Show, Eq)

data Edge   = Links
            | Rechts

type Node   = [Edge]

--  Folien 18 - 30
update :: Eq a => (a -> b) -> a -> b -> a -> b
update f a b a' =   if a == a' 
                    then b
                    else f a'

lift :: (a -> b -> c) -> (state -> a) -> (state -> b) -> (state -> c)
lift op f g state = f state `op` g state

fact :: Int -> Int
fact n =    if n > 1
            then n * fact (n - 1)
            else 1

factI :: Int -> Int
factI n = loop1 1 n

loop1 :: Int -> Int -> Int
loop1 state n =  if n > 1
                then loop1 (n * state) (n - 1)
                else state


hoch :: (a -> a) -> Int -> a -> a
f `hoch` n =    if n == 0
                then id
                else f.(f`hoch`(n - 1))

updList :: [a] ->Int -> a -> [a]
updList s i a = take i s ++ a : drop (i + 1) s

loop2 :: [a] -> [a] -> [a]
loop2 state (a:s) = loop2 (a:state) s
loop2 state _     = state

reverseI :: [a] -> [a]
reverseI = loop2 []

--  Folien 31 - 33

sublist :: [a] -> Int -> Int -> [a]
sublist (a:_) 0 0                   = [a]
sublist (a:s) 0 j   |j > 0          = a : (sublist s 0 $ j-1)   --() were left out in the lecture
sublist (_:s) i j   |i > 0 && j > 0 = sublist s (i-1) $ j-1
sublist _     _ _                   = []

merge :: [Int] -> [Int] -> [Int]
merge s1@(x:s2) s3@(y:s4)   |x < y  = x : merge s2 s3
                            |y < x  = y : merge s1 s4
                            |True   = merge s1 s4
merge [] s = s
merge s  _ = s

-- Folien 34 - 35

type ListIndex  a = ([a], Int)
type ListZipper a = ([a], [a])

listToZipper :: ListIndex a -> ListZipper a
listToZipper =  loop []
                where
                loop :: [a] -> ([a], Int) -> ([a], [a])
                loop c (s,0)    = (c,s)
                loop c (a:s,n)  = loop (a:c) (s,n-1)

zipperToList :: ListZipper a -> ListIndex a
zipperToList (c,s) =    loop c (s,0)
                        where
                        loop :: [a] -> ([a],Int) -> ([a], Int)
                        loop (a:c) (s,n)    = loop c (a:s,n+1)
                        loop _ sn           = sn

back, forth :: ListZipper a -> ListZipper a
back (a:c,s) = (c,a:s)

forth(c,a:s) = (a:c,s)

--  Folien 36 - 40

updRel :: [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d    =   if a == c
                            then (a,d) : r
                            else (a,b) : updRel r c d
updRel _         a b    = [(a,b)]

--  Folien 40 - 50 

fold2 :: (state -> a -> b-> state) -> state -> [a] -> [b] -> state
fold2 f state (a:as) (b:bs)     = fold2 f (f state a b) as bs
fold2 _ state _      _          = state 

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs $ tail fibs

--primes :: [Int]
--primes = sieve $ nats 2
--
--sieve :: [Int] -> [Int]
--sieve (p:s) = p : sieve [n | n <- s, n `mod` p /= 0]
--sieve (p:s) = p : sieve (filter ((/= 0). (`mod` p) s)

--

foldTree ::  (a -> val) -> (a -> valL -> val) -> valL
                                -> (val -> valL -> valL) -> Tree a -> val
foldTree f g _ _ (V a)      = f a
foldTree f g nil h (F a ts) = g a $ foldTrees f g nil h ts

foldTrees ::  (a -> val) -> (a -> valL -> val) -> valL
                                -> (val -> valL -> valL) -> [Tree a] -> valL
foldTrees _ _ nil _ []      = nil
foldTrees f g nil h (t:ts)  = h (foldTree f g nil h t) 
                                (foldTrees f g nil h ts)
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
