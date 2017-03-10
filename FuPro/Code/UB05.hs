module UB05 where

--a5.1

double :: [Int] -> [Int]
double list = map (*2) list

maxima :: [Int] -> [Int] -> [Int] 
maxima list1 list2 = zipWith max list1 list2

funs :: Int -> [Int]
funs x = map ($ x) [(+1), (*2), (^2)]

toUnicode :: String -> [Int]
toUnicode str = map fromEnum str


--a5.2
--
-- foldl (-) 5 [1,3]
-- foldl (-) 4 [3]
-- foldl (-) 1 []
-- 1 
--
-- foldr (-) 5 [1,3]
-- 1 - foldr (-) 5 [3]
-- 1 - $ 3 - foldr (-) 5 []
-- 1 - $ -2
-- 3

--a5.3

inBoth :: Eq a => [a] -> [a] -> [a]
inBoth as  bs = [m | m <- as, elem m bs]

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = [f a b | a <- as, b <- bs]

divisors :: Int -> [Int]
divisors n = [x | x <-[1..n], n `mod` x == 0]

solutiolns :: [(Int, Int, Int)]
solutiolns 
        = [(x,y,z) |x <- l, y <- l, z <- l, 3 * x^2 + 2 * y + 1 == z]
                where
                l = [0..100]
