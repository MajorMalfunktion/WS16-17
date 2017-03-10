module UB01 where

addFive :: Int -> Int
addFive x = x + 5

one :: Int
one = 1

k :: Int
k = addFive (addFive one)

addTenComp :: Int -> Int
addTenComp x = (addFive . addFive) x

func :: Int 
func = (\y -> (\x -> x+1) (4+y))6

--  (\y -> (\x -> x+1) (4+y))6
--  (\x -> x+1) 10
--  11
