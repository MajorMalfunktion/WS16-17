{-# LANGUAGE GADTs, LambdaCase, TypeSynonymInstances, FlexibleInstances #-}
module Vlfun where
import Control.Monad

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

single :: a -> [a]
single a = [a]

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

subset :: Eq a => [a] -> [a] -> Bool
subset s s' = all (flip elem s') s

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldl union [] . map f

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

-- Folien 126 - 

fixpt :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpt le phi a
        | le (phi a) a  = a
        | otherwise     = fixpt le phi (phi a)

-- Folien 130 -

--liftM2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
--liftM2 operation f g x = operation (f x) (g x)

-- dfining own Semiring class
class Semiring r where
    add, mul :: r -> r -> r
    zero, one :: r

-- instances for Bool and Int
instance Semiring Bool where
    add = (||); mul = (&&)
    zero = False; one = True
    
instance Semiring Int where
    add = (+); mul = (*)
    zero = 0; one = 1

-- Binary Relation
type BinRel a = [(a,a)]

type BRfun a = a -> [a]

-- instances of Eq in Semiring for BRfun and TRfun

instance Eq a => Semiring (BinRel a) where
    add = union
    mul r r' =  [(a,c) | (a,b) <- r, (b',c) <- r', b == b']
    zero = []
    one = undefined

instance Eq a => Semiring (BRfun a) where
    add sucs sucs' = liftM2 union sucs sucs'
    mul sucs sucs' = unionMap sucs' . sucs
    zero = const []; one = single
--

type TRfun a label = a -> [(a,label)]

data Graph a = G [a] (BRfun a)

data GraphL a label = GL [a] (TRfun a label)

-- Folien 131
-- take a Graph and return a Graph with a changed (BRfun a) sucs'
closureF, closureT{-, warshall -}:: Eq a => Graph a -> Graph a

-- returns a new (BRfun a) sucs', 
-- of whom elements are a subset to a prior (BRfun a) sucs
closureF (G nodes sucs) 
        = G nodes sucs'
        where
        -- phi adds the one element and multiplies BRfun 
        phi = (mul sucs . add one)
        -- the first element is the zero element
        a = zero
        sucs'           = fixpt le phi a
        -- le takes two funtions and checks for all elements,
        -- if their results are subsets
        le sucs sucs''  = all (liftM2 subset sucs sucs'') nodes

closureT (G nodes sucs)
        = G nodes sucs'
        where
        sucs' = mul sucs $ add one sucs'

--warshall (G nodes sucs)
--        = G nodes sucs'
--        where
--        sucs'        = foldl trans sucs nodes
--        trans sucs a = fold2 update sucs nodes $ map f nodes 
--            where
--            f b | a elem (sucs b) = union (sucs b) (sucs a)
--                | otherwise       = sucs b

-- Folien 158 -
-- mzero ist eine gescheiterte Berechnung (verglischbar mit Bottom/ Null/ etc. ..)
-- mplus ist eine simple monadische Funktion
--
--class Monad m => MonadPlus m where
--    mzero :: m a
--    mplus :: m a -> m a -> m a

-- Folien 161 -

--guard :: MonadPlus m  => Bool -> m ()
--guard b | b         = return ()
--        | otherwise = mzero

creturn :: MonadPlus m => (a -> Bool) -> a -> m a
creturn f a 
        =   do
            guard $ f a
            return a

when :: Monad m => Bool -> m () -> m()
when bool mon
        | bool      = mon
        | otherwise = return ()

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< mon = mon >>= f

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (>>= f) . g

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = flip (Vlfun.<=<)

join :: Monad m => m (m a) -> m a
join = (>>= id)

-- Folien 163 -

some, many :: MonadPlus m => m a -> m [a]
some mon = do a <- mon; as <- many mon; return $ a:as
many mon = mplus (some mon) (return [])

msum :: MonadPlus m => [m a] -> m a
msum = foldr mplus mzero

sequence :: Monad m => [m a] -> m [a]
sequence (m:ms) = do a <- m; as <- Vlfun.sequence ms; return $ a:as
sequence _      = return []
--        = \case (m:ms)  -> do a <- m; as <- Vlfun.sequence ms; return $ a:as
--                _       -> return []

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) $ return ()

-- map und zipWith fuer Monaden:

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = Vlfun.sequence . map f

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = Vlfun.sequence_ . map f

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f as = Vlfun.sequence . zipWith f as

zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f as = Vlfun.sequence_ . zipWith f as

-- Folien 173 - 

queens :: Int -> [[Int]]
queens n = boardVals [1..n]

boardVals :: [Int] -> [[Int]]
boardVals [] = [[]]
boardVals l  = [new | k <- l, val <- boardVals $ remove k l, let new = k : val, safe 1 new]

safe :: Int -> [Int] -> Bool
safe i (k:col:val)  = col - 1 /= k
                    && col + 1 /= k
                    && safe (i+1) (k:val)
safe _ _            = True

-- Folien 188 - 

--instance Monad (Trans state) where
--    return a    = T $ \st -> (a,st)
--    T h >>= f   = T $ (\(a,st) -> runT (f a) st) . h

-----------------------------------------------------------------------------
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
