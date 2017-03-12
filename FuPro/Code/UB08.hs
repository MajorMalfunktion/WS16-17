module UB08 where
import Vlfun

--a8.1

class Addition a where
    add :: a -> a -> a

instance Addition Nat where
    add Zero      x     = x
    add x         Zero  = x
    add (Succ x') x     = Succ (add x' x)

instance Addition PosNat where
    add One        x    = Succ' x
    add x          One  = x
    add (Succ' x') x    = Succ' (add x' x)

--a8.2

instance Eq Nat where
    Zero    == Zero     = True
    _       == _        = False
    Succ m  == Succ n   = m == n

instance Ord Nat where
    Zero    <= _        = True
    Succ n  <= Succ m   = n <= m
    _       <= _        = False

instance Enum Nat where
    toEnum n
        | n > 0     = Succ $ toEnum (n-1)
        | otherwise = Zero
    fromEnum n 
        = case n of
            Zero -> 0
            Succ m -> 1 + fromEnum m

instance Show Nat where
    show = show.fromEnum

instance Num Nat where
    negate  = undefined
    abs n   = n
    signum Zero = Zero
    signum n    = Succ Zero
    fromInteger = toEnum . fromInteger
-- easiest way to get it done, but unefficient
--    (+) m n = toEnum $ fromEnum m + fromEnum n 
--    (*) m n = toEnum $ fromEnum m * fromEnum n 
--  (+)
    Zero + n = n
    n + Zero = n
    Succ n + Succ m = Succ (n + m)
--  (*)
    Zero * _ = Zero
    _ * Zero = Zero
    Succ n * i = i + (n*i)

solutions :: [(Nat,Nat,Nat)]
solutions = [(x,y,z) | z <- [0..], x <- [0..z], y <- [0..z], 3 * x^2 + 2 * y + 1 == z]

--a8.3

symDif :: Eq a => [a] -> [a] -> [a] 
symDif as bs = union (diff as bs) (diff bs as)

height :: Bintree a -> Int
height tree
        = case tree of
            Empty        -> 0
            (Fork _ l r) -> 1 + max (height l) (height r) 

isBalanced :: Bintree a -> Bool
isBalanced tree
        = case tree of
            Empty        -> True
            (Fork _ l r) -> (f l r <= 1) && isBalanced l && isBalanced r
                where f = (\x y -> abs (height x - height y))
