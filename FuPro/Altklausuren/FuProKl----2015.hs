{-# LANGUAGE LambdaCase #-}
module FuProKl2015 where

--A1

--1.
a11 :: [Integer] -> [[Integer]]
a11 = (\x -> [x,x]) . filter (\x -> x > 2)

--a22 :: 
--a22 = 

--2.
a12a = (\f -> (\x -> x+10) .f.f.f) (\x -> x*2) 3
-- (...+10) . (..*2) . (...) . (..*2) 3
-- 34

a12b = foldl (\x y -> if y `mod` 2 /= 0 then y else x) 0 [4,3,2,1]
-- foldl (...) 0 [3,2,1]
-- foldl (...) 3 [2,1]
-- ..
-- 1

--A2

delete' :: Eq a =>  a -> [a] -> [a]
delete' trg []     = []
delete' trg (a:as) = case trg == a of
    True -> as
    False -> a : delete' trg as

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _  = True
isPrefixOf' _  [] = False
isPrefixOf' (a:as) (b:bs) = case a == b of
    True -> isPrefixOf' as bs
    False -> False

--A3

data Tree = Tree Int [Tree]

blattString :: Tree -> [Int]
blattString (Tree i []) = [i]
blattString (Tree i as) = [i] ++ (concat $ map blattString as)

isBinaryTree :: Tree -> Bool
isBinaryTree (Tree i ts) = (len ts <= 2) && all isBinaryTree ts
    where
    len []      = 0
    len (a:as)  = 1 + len as

subBySum :: Tree -> Tree
subBySum tree = gewicht tree 0
    where
    gewicht (Tree i []) gew = (Tree gew [])
    gewicht (Tree i ts) gew = do
        let newGew = (len ts) + gew
        let newTs  = map (flip gewicht newGew) ts
        (Tree gew newTs)
    len []      = 0
    len (a:as)  = 1 + len as

--A4

data Formula = Atom String 
             | Conj Formula Formula

--1.

atoms :: Formula -> [String]
atoms f = noDubs (help f) 
    where
    help = \case
        Atom a      -> [a]
        Conj f1 f2  -> (atoms f1) ++ (atoms f2)

noDubs :: [String] -> [String]
noDubs []     = []
noDubs (a:as) = a : noDubs (filter (not . sameString a) as)

sameString :: String -> String -> Bool
sameString [] [] = True
sameString _  [] = False
sameString [] _  = False
sameString (a:as) (b:bs)
    | a == b    = sameString as bs
    | otherwise = False

--2.

models :: [String] -> Formula -> Bool
models arg form = any (sameString (head arg)) (atoms form)

--3.
type Rule = (Formula, Formula)

deduce :: [String] -> Rule -> Maybe [String]
deduce base (a,b) = if models base a then Just (noDubs (base ++ atoms b) ) else Nothing

--A7

--2.

data NeverEndingBin a = Knot a (NeverEndingBin a) (NeverEndingBin a)
