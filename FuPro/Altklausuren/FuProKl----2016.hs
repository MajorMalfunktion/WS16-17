{-# LANGUAGE LambdaCase#-}
module FuProKl2016 where
import DynProg
import Data.Array

--A1

--1.
a11 :: ([Integer], a) -> [Integer]
a11 = map (\x -> x+1) . fst

a12 :: [a] -> [a] -> Maybe [a]
a12 [] [] = Just []
a12 (x:xs) (y:ys) = case a12 xs ys of
    Nothing -> Nothing
    Just zs -> Just (x:y:zs)
a12 _  _ = Nothing 

--2.
data Either a b = Left a
                | Right b

-- Either :: * -> * -> *
-- Either Int :: * -> *

--3.

a13 = (\x -> x+1) $ (\f -> f . f) (\x -> x * 3) 2
-- (\x -> x+1) $ ((\x -> x * 3) . (\x -> x * 3)) 2
-- (\x -> x+1) $ ((\x -> x * 3) 6
-- (\x -> x+1) 18
-- 19

--A2

--1.
any' :: (a -> Bool) -> [a] -> Bool
any' f ls = case ls of
    (a:as) -> f a || any' f as
    []     -> False

--2.
p :: (a -> Bool) -> [a] -> ([a],[a])
p f ls = (takeW f ls, dropW f ls)
    where
    takeW f ls = case ls of
        []      -> []
        (a:as)  -> if f a then a : takeW f as else []
    dropW f ls = case ls of
        []      -> []
        (a:as)  -> if f a then dropW f as else (a:as)

--A3

--data Tree v e = Node v [(e, Tree v e)]
--
--preorder :: Tree v e -> [v]
--preorder = \case
--    (Node v [])         -> [v]
--    (Node v [t])   -> v : map preorder' t
--    where
--    preorder' = \case
--        (e,t) -> preorder t
--
--rev :: Tree v e -> Tree v e
--rev = \case
--    (Node v []) -> Node v []
--    (Node v ts) -> reverse (map rev ts)

--paths :: Tree v e -> [[e]]
--ascPath :: Ord e => Tree v e

--A4

--1.
data IExp   = Const Int
            | Sum IExp IExp
            | Division IExp IExp

eval :: IExp -> Maybe Int
eval = \case
    (Const i)        -> Just i
    (Sum e1 e2)      -> (eval e1) >>= myMult (eval e2)
    (Division e1 e2) -> if (eval e2) == Just 0
        then Nothing
        else eval e1 >>= myDiv (eval e2)
    where
    myMult :: Maybe Int -> Int -> Maybe Int
    myMult may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x * i)
    myDiv :: Maybe Int -> Int -> Maybe Int
    myDiv may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x `div` i)

evalDo :: IExp -> Maybe Int
evalDo exp = do
    case exp of 
        (Const i) -> do
            Just i
        (Sum e1 e2) -> do
            ((eval e1) >>= myMult (eval e2))
        (Division e1 e2) -> do
            if (eval e2) == Just 0  
                then Nothing
                else (eval e1 >>= myDiv (eval e2))
    where
    myMult :: Maybe Int -> Int -> Maybe Int
    myMult may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x * i)
    myDiv :: Maybe Int -> Int -> Maybe Int
    myDiv may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x `div` i)
    

xs,xs1, xs2 :: [Int]
xs = [x+y | x <- [1,5], y <- [10,20]]

xs1 = do
    x <- [1,5]
    y <- [10,20]
    return (x+y)

xs2 = 
    [1,5] >>= \x ->
    [10,20] >>= \y -> 
    return (x+y)

--A5

fDyn :: Int -> Int
fDyn n = arr ! n
    where
    arr = mkArray f (0,1000000)
        where
        f 0 = 1
        f 1 = 3
        f n = sum [arr ! i| i <- [0..(n-1)]]

--A6
--
--inc :: Trans (String -> Int,Int) ()
--
--upd :: String -> Int -> Trans (String -> Int,Int)
--
--eval :: Statement -> Trans (String )
