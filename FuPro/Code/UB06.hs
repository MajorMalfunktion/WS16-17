module UB06 where
import Vlfun 

--a6.1
--
-- iterate (*2) 3 !! 2
-- 3 : (iterate (*2) ((*2) 3)) !! 2
-- (iterate (*2) ((*2) 3)) !! 1
-- ((*2) 3) : (iterate (*2) ((*2) ((*2) 3))) !! 1
-- (iterate (*2) ((*2) ((*2) 3))) !! 0
-- ((*2) ((*2) 3)) : (iterate (*2) ((*2) ((*2) ((*2) 3)))) !! 0
-- ((*2) 6) 
-- 12

solutiolns :: [(Int,Int,Int)]
solutiolns = [(x,y,z) | z <- [0..], x <- [0..z], y <- [0..z], 3 * x^2 + 2 * y + 1 == z]

--a6,2

data Rat = Rat Int' PosNat
         deriving (Show)

c :: Rat
c = Rat (Minus One) (Succ' One)

--a6.3

indexNat :: [a] -> Nat -> a
indexNat (a:as) (Succ n) 
        | (Succ n) == Zero  = a
        | otherwise         = indexNat as n

colistTake :: Int -> Colist a -> Colist a
colistTake _ (Colist Nothing)   = Colist Nothing
colistTake n (Colist (Just (a,b)))
        | n > 0     = Colist (Just (a, colistTake (n-1) b))
        | otherwise = Colist Nothing

getX :: Point' -> Float
getX (Point' x _ _) = x

setX :: Float -> Point' -> Point'
setX x (Point' _ y z) = Point' x y z

--a6,4

to :: [()] -> Nat
to []       = Zero
to ([]:xs)  = Succ (to xs)

from :: Nat -> [()]
from Zero       = []
from (Succ n)   = () : from n
