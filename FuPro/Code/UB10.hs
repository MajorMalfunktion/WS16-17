module UB10 where
import Vlfun

--A10.1

product_ :: Num a => Tree a -> a
product_ = foldTree id (*) 1 (*)

inorderB :: Bintree a -> [a]
inorderB = foldBTree [] (\a l r -> l ++ [a] ++ r)

--A10.2

foldNat :: val -> (val -> val) -> Nat -> val
foldNat nil f nat
    = case nat of
        Zero    -> nil
        Succ n  -> f $ foldNat nil f n

toInt :: Nat -> Int
toInt = foldNat 0 (1+)

--A10.3

expr :: Exp String
expr = Sum [3 :* Var "x", Con 5]

vars :: Store String
vars "x" = 2

getResult :: State x -> Int
getResult = head . fst

-- 3, x laden
-- Push 3   -> [3]
-- Load "x" -> [2,3]
-- 2 Zahlen multiplizieren
-- Mul 2    -> [6]
-- 5 laden
-- Push 5   -> [6,5]
-- 2 Zahlen addieren
-- Add 2    -> [11]

class Hash a where
    hash :: a -> Int

instance Hash a => Hash [a] where
    hash list
        = case list of
            []      -> 3 ^ 2
            (a:as)  -> 3 * (3 + hash a) + hash as
                
instance Hash Nat where
    hash nat 
        = case nat of
            Zero    -> 3
            Succ n  -> 3 + hash n

instance Hash a => Hash (Tree a) where
    hash tree
        = case tree of
            V a     -> 3 * (3 + hash a)
            F a ls  -> 3 * (3 + hash a) + hash ls
