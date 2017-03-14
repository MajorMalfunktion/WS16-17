{-# LANGUAGE GADTs #-}
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

updList :: [a] -> Int -> a -> [a]
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

updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d    =   if a == c
                            then (a,d) : r
                            else (a,b) : updRel r c d
updRel _         a b    = [(a,b)]

--  Folien 40 - 50 

fold2 :: (state -> a -> b-> state) -> state -> [a] -> [b] -> state
fold2 f state (a:as) (b:bs)     = fold2 f (f state a b) as bs
fold2 _ state _      _          = state 

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

nats :: Int -> [Int]
nats n = n : map (+1) (nats n)

primes :: [Int]
primes = sieve $ nats 2

sieve :: [Int] -> [Int]
sieve (p:s) = p : sieve [n | n <- s, n `mod` p /= 0]
--sieve (p:s) = p : sieve (filter ((/= 0). (`mod` p)) s)

--  Folien 51 -

type Path = [Point]
type Point = (Float, Float)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

length :: Path -> Float
length ps = sum $ zipWith distance ps $ tail ps

straight :: Point -> Point -> Point -> Bool
straight (x1, y1) (x2, y2) (x3, y3) =   x1 == x2 && x1 == x3 
                                    ||  x1 /= x2 && x2 /= x3 
                                        && (y2 - y1) / (x2 - x1) 
                                        == (y3 - y2) / (x3 - x2)

minimize :: Path -> Path
minimize (p:ps @(q:r:s))| straight  p q r   = minimize $ p:r:s
                        | True              = p : minimize ps 
minimize ps                                 = ps


-- Folie 60 - 

data Nat    = Zero
            | Succ Nat
--            deriving (Show, Eq)

data PosNat = One
            | Succ' PosNat
            deriving (Show, Eq)

data Int'   = Zero'
            | Plus PosNat
            | Minus PosNat
            deriving (Show, Eq)

-- data Colist a = {split :: Maybe (a,Colist a)}
data Colist a = Colist (Maybe (a, Colist a))

nil :: Colist a
nil = Colist Nothing

--data Point' = Point {x,y,z :: Float}
data Point' = Point' Float Float Float


-- Folie 70 -

--data Exp x  
--        = Con Int
--        | Var x
--        | Sum [Exp x]
--        | Prod [Exp x]
--        | Exp x :- Exp x
--        | Int :* Exp x
--        | Exp x :^ Int

data Exp x 
        where
        Con :: Int -> Exp x
        Var :: x -> Exp x
        Sum :: [Exp x] -> Exp x
        Prod:: [Exp x] -> Exp x
        (:-):: Exp x -> Exp x -> Exp x
        (:*):: Int -> Exp x -> Exp x
        (:^):: Exp x -> Int -> Exp x

--data BExp x 
--        = True_
--        | False_
--        | BVar x
--        | Or [BExp x]
--        | And [BExp x]
--        | Not (BExp x)
--        | Exp x := Exp x 
--        | Exp x :<= Exp x

data BExp x 
        where
        True_   :: BExp x
        False_  :: BExp x
        BVar    :: x -> BExp x
        Or      :: [BExp x] -> BExp x
        And     :: [BExp x] -> BExp x
        Not     :: BExp x -> BExp x
        (:=)    :: Exp x -> Exp x -> BExp x
        (:<=)   :: Exp x -> Exp x -> BExp x

type Store x = x -> Int

exp2store :: Exp x -> Store x -> Int
--exp2store (Con i)   _   = i
--exp2store (Var x)   st  = st x
--exp2store (Sum es)  st  = sum $ map (flip exp2store st) es
--exp2store (Prod es) st  = product $ map (flip exp2store st) es
--exp2store (e :- e') st  = exp2store e st - exp2store e' st
--exp2store (e :* i)  st  = exp2store e st * i
--exp2store (e :^ i)  st  = exp2store e st ^ i
exp2store expr st 
        = case expr of
            Con i       -> i
            Var x       -> st x
            Sum es      -> sum $ map f es
            Prod es     -> product $ map f es
            e :- e'     -> f e - f e'
            i :* e      -> i * f e 
            e :^ i      -> f e ^ i
        where
        f = (flip exp2store st)

conEqInt :: Exp x -> Int -> Bool
conEqInt (Con c) i = c == i

intEqCon :: Int -> Exp x -> Bool
intEqCon = flip conEqInt 

-- Folien 81 -

union :: Eq a => [a] -> [a] -> [a]
union = foldl $ flip insert

diff :: Eq a => [a] -> [a] -> [a]
diff = foldl $ flip remove

insert :: Eq a => a -> [a] -> [a]
insert a s@(b:s')
        | a == b    = s
        | otherwise = b : insert a s'
insert a _          = [a]

remove :: Eq a => a -> [a] -> [a]
remove = filter . (/=)

-- Folien 109 - 

foldBTree :: val -> (a -> val -> val -> val) -> Bintree a -> val
foldBTree val f tree
        = case tree of
            Empty               -> val
            Fork a left right   -> f a (foldBTree val f left) (foldBTree val f right)

foldTree ::  (a -> val) -> (a -> valL -> val) -> valL -> (val -> valL -> valL) -> Tree a -> val
foldTree f _ _   _ (V a)    = f a
foldTree f g nil h (F a ts) = g a $ foldTrees f g nil h ts

foldTrees ::  (a -> val) -> (a -> valL -> val) -> valL -> (val -> valL -> valL) -> [Tree a] -> valL
foldTrees _ _ nil _ []      = nil
foldTrees f g nil h (t:ts)  = h (foldTree f g nil h t) (foldTrees f g nil h ts)

-- Folien 114 -

type State x = ([Int], Store x)

-- Folien 131 -

type BRfun a = a -> [a]

data Graph a = G [a] (BRfun a)

-- Folien 158 -
--
--class Monad m => MonadPlus m where
--    mzero :: m a
--    mplus :: m a -> m a -> m a
---------------------------------------------------------------------------
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
