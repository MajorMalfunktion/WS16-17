{-# LANGUAGE LambdaCase #-}
module Klausur09022013 where
import Data.List
import Data.Maybe

--A1

--Typen bestimmen

g :: [a] -> Maybe [a]
g [] = Nothing
g (x:xs) = Just xs

h :: [(Integer -> Integer)] -> [Bool]
h = map (\g -> g 1 < 2)

--A2

a =  (\x -> (\x -> x * x) 2 + 2) 2
-- (\x -> (2 * 2) + 2) 2
-- (\x -> 6) 2
-- 6

b = map (\g -> g 5) $ zipWith (.) 
    [(\x -> x * x), (\x -> x + 3), id]
    [(id), (\x -> x + 3), (\x -> x * x)]
-- map (\g -> g 5) 
--  [(\x -> x * x) . id, 
--      (\x -> x + 3) . (\x -> x + 3),
--      (\x -> x * x) . id]
--  [(\x -> x * x) . id 5, 
--      (\x -> x + 3) . (\x -> x + 3) 5,
--      (\x -> x * x) . id 5]
--  [(\x -> x * x) 5, 
--      (\x -> x + 3) 8,
--      (\x -> x * x) 5]
--  [25, 11, 25]

--A3

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f ls = case ls of
    []      -> []
    (a:as)  -> if f a then a : takeWhile f as else []

isSubstringOf :: String -> String -> Bool
isSubstringOf [] _  = True
isSubstringOf _  [] = False
isSubstringOf as (b:bs)
    = isPrefixOf as (b:bs) || isSubstringOf as bs

--A4

data MyBool = WAHR
            | FALSCH

data BE = BE MyBool
        | NEG BE 
        | DIS BE BE
        | KON BE BE

eval :: BE -> Bool
eval = \case 
    (BE WAHR)   -> True
    (BE FALSCH) -> False
    (NEG val)   -> not $ eval val
    (DIS v1 v2) -> (||) (eval v1) (eval v2)
    (KON v1 v2) -> (&&) (eval v1) (eval v2)

--A5
--1. instance Eq (Tree a) hinzufuegen

data Tree a = Tree a [Tree a]
            deriving (Show)

instance (Eq a) => Eq (Tree a) where
    Tree a []   == Tree b []    = a == b
    Tree a _    == Tree b []    = False
    Tree a []   == Tree b _     = False
    Tree a as   == Tree b bs    = a == b && and $ zipWith (==) as bs

-- Testfaelle
tree1 = Tree 5 [Tree 6 [Tree 8 [], Tree 9 []]]
tree2 = Tree 5 [Tree 6 [Tree 8 [], Tree 9 []], Tree 3 []]
tree3 = Tree 5 [Tree 6 [Tree 8 [], Tree 8 []], Tree 3 []]

trueTree, falseTree1, falseTree2, falseTree3 :: Bool
trueTree    = tree3 == tree3
falseTree1  = tree3 == tree1
falseTree2  = tree3 == tree2
falseTree3  = tree1 == tree2

--2. instance Functor Tree hinzufuegen

instance Functor Tree where
    fmap f (Tree a []) = Tree (f a) []
    fmap f (Tree a ls) = Tree (f a) (map (fmap f) ls)

--3. zipTree

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Tree a []) (Tree b _)  = Tree (a,b) []
zipTree (Tree a _) (Tree b [])  = Tree (a,b) []
zipTree (Tree a as) (Tree b bs) = Tree (a,b) (zipTree2 as bs)
    where
    zipTree2 _      []     = []
    zipTree2 []     _      = []
    zipTree2 (a:as) (b:bs) = zipTree a b : zipTree2 as bs

--A6

sqrtSafe:: Float -> Maybe Float
sqrtSafe x
   | x < 0      = Nothing
   | otherwise  = Just $ sqrt x

headSafe:: [a] -> Maybe a
headSafe []     = Nothing
headSafe (x:_)  = Just x

--1.

sqrtHead :: [Float] -> Maybe Float
sqrtHead xs = headSafe xs >>= sqrtSafe

--2.

capitalA :: String -> String -> IO ()
capitalA src trg 
    =   do
        file <- readFile src
        capi <- (return . allA) file
        writeFile trg capi
    where
    allA = \case
        []      -> []
        (a:as)  -> if a == 'a' then 'A' : allA as else a : allA as
