module UB03 where

--a3.1

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

--a3.2

a32 = ((x * y) * z) +3
b32 = ((add3 1) 2) 3
c32 = f (g . (h x))

--a3.3
--
-- 5 * 5
-- 25
--
-- (+3) 8
-- 11

--a3.4
-- 1)
-- es muss gelten a :: Int
-- damit (5+) a :: Int
--
-- 2)
-- Typfehler
--
-- 3)
-- h :: a -> b
-- g :: b -> c
-- f :: a -> c
