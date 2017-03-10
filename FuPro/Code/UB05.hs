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
