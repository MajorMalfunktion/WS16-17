module UB02 where

--2.1 

a21 :: Num a => a -> a
a21 a = (\x -> 5 * x^2 + 14 * x +6) a

b21 :: Num a => a -> a
b21 a = (\x -> 56 * x^3 + 9 * x) a

--2.2
--
--nicht auswertbar sind b,d

--2.3

a23 :: Int -> Int
a23 = \x -> x + 1

b23 :: (Int,Int) -> Int
b23 = \(x,y) -> x * y
