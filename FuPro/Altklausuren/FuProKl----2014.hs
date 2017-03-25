{-# LANGUAGE LambdaCase #-}
module FuProKl2014 where
import Data.Maybe
import DynProg
import Data.Array

--A1
--1.
a1a :: ([Integer], a) -> [Integer]
a1a = map (\x -> x + 1) .fst

a1b :: Maybe (a -> Bool) -> [a] -> Bool
a1b (Just f) (x:xs) = (f x) && (a1b (Just f) xs)
a1b _        _      = True

--2.
a2 = map (\(x,y) -> x + y) $ zip [0,1..] [1..4]
-- map (\(x,y) -> x + y) [(0,1),(1,2),(2,3),(3,4)]
-- 1 : map (\(x,y) -> x + y) [(1,2),(2,3),(3,4)]
-- 1 : 3 : 5 : 7 : []
-- [1,3,5,7]

--1.
data Either a b = Left a 
                | Right b
data D a b = C b (a Int) b

--a)
--
--Either [Int] :: * -> *
--
-- D :: (* -> *) -> * -> *


--A2

insert :: Ord a => a -> [a] -> [a]
insert a ls = case ls of
    []      -> [a]
    (b:bs)  -> if (a > b) then b : insert a bs else a:(b:bs)

concat' :: [[a]] -> [a]
concat' []      = []
concat' (a:as) = a ++ concat' as

--A3

data Bintree a  = Empty
                | Fork a (Bintree a) (Bintree a)

--1.
preorder :: Ord a => Bintree a -> [a]
preorder = \case
    Empty         -> []
    (Fork a l r)  -> a : preorder l ++ preorder r

--2.
zipTree :: Bintree a -> Bintree b -> Bintree (a,b)
zipTree Empty           _               = Empty
zipTree _               Empty           = Empty
zipTree (Fork a1 l1 r1) (Fork a2 l2 r2) = Fork (a1, a2) 
                                            (zipTree l1 l2) 
                                            (zipTree r1 r2)

--3.
instance Functor Bintree where
    fmap _ Empty        = Empty
    fmap f (Fork a l r) = Fork (f a) (fmap f l) (fmap f r)

--4.
isBinSearchTree :: Ord a => Bintree a -> Bool
isBinSearchTree = \case
    Empty        -> True
    (Fork a l r) -> rightOrder a l r
                    && isBinSearchTree l 
                    && isBinSearchTree r
    where
    rightOrder a Empty        Empty        = True
    rightOrder a (Fork l _ _) (Fork r _ _) = l < a && a < r 
    rightOrder a _            (Fork r _ _) = a < r
    rightOrder a (Fork l _ _) _            = l < a

--A4
data Exp    = Const Int
            | Var Char
            | Negation Exp
            | Sum Exp Exp
            | Product Exp Exp

eval1, eval2 :: Exp -> (Char -> Maybe Int) -> Maybe Int

-- Wie ich es machen wuerde

eval1' exp f = case exp of
    (Const i)       -> Just i
    (Var c)         -> f c
    (Negation e)    -> myMult (eval1 e f) (-1)
    (Sum e1 e2)     -> (eval1 e1 f) >>= mySum (eval1 e2 f)
    (Product e1 e2) -> (eval1 e1 f) >>= myMult (eval1 e2 f)
    where
    myMult may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x * i)
    mySum may = case may of 
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ i + x) 

--

eval2' exp f = do
    case exp of
        (Const i)       -> Just i
        (Var c)         -> f c
        (Negation e)    -> myMult (eval1 e f) (-1)
        (Sum e1 e2)     -> (eval1 e1 f) >>= mySum (eval1 e2 f)
        (Product e1 e2) -> (eval1 e1 f) >>= myMult (eval1 e2 f)
    where
    myMult may = case may of
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ x * i)
    mySum may = case may of 
        Nothing  -> (\x -> Nothing)
        (Just i) -> (\x -> Just $ i + x) 

--1.

eval1 (Var c)         f = f c
eval1 (Const i)       f = Just i
eval1 (Negation e)    f = case (eval1 e f) of
    Nothing  -> Nothing
    (Just i) -> Just $ i * (-1)
eval1 (Sum e1 e2)     f = case (eval1 e1 f) of
    Nothing -> Nothing
    (Just n)  -> case (eval1 e2 f) of
        Nothing  -> Nothing
        (Just m) -> Just $ n + m
eval1 (Product e1 e2) f = case (eval1 e1 f) of
    Nothing -> Nothing
    (Just n)  -> case (eval1 e2 f) of
        Nothing  -> Nothing
        (Just m) -> Just $ n * m

--2.

eval2 = eval2'


--A5

data Maze   = Exit
            | DeadEnd
            | Branch Maze Maze

data Direction  = LeftD
                | RightD

type Way = [Direction]

--1.

follow :: Maybe Way -> Maze -> Maybe Maze
follow way maze = case way of
    Nothing -> Nothing
    Just w  -> walkThrough w maze
    where
    walkThrough []     maze = Just maze
    walkThrough (w:ws) maze = case maze of
        Exit    -> Nothing
        DeadEnd -> Nothing
        (Branch l r) -> case w of
            LeftD  -> walkThrough ws l
            RightD -> walkThrough ws r

--2.

shortestPaths :: Maybe Way -> Maybe Way -> Maybe Way
shortestPaths Nothing   Nothing    = Nothing
shortestPaths p         Nothing    = p
shortestPaths Nothing   p          = p
shortestPaths (Just p1) (Just p2)  
    = if shorter p1 p2 
        then (Just p1) 
        else (Just p2)
    where
    shorter [] _          = True
    shorter _  []         = False
    shorter (a:as) (b:bs) = shorter as bs

--3.

shortest :: Maze -> Maybe Way
shortest = shortestHelp []

shortestHelp :: Way -> Maze -> Maybe Way
shortestHelp w m = case m of 
    DeadEnd     -> Nothing
    Exit        -> Just $ reverse w
    Branch l r  -> do
        let lW = shortestHelp (LeftD:w) l
        let rW = shortestHelp (RightD:w) r
        if isShorter lW rW then lW else rW
    where
    isShorter Nothing    Nothing    = True
    isShorter _          Nothing    = True
    isShorter Nothing    _          = False
    isShorter (Just(as)) (Just(bs)) = fastShort as bs
    fastShort []     [] = True
    fastShort _      [] = False
    fastShort []     _  = True
    fastShort (a:as) (b:bs) = fastShort as bs

--A6

dynFac :: Int -> Int
dynFac n = arr ! n
    where
    arr =  mkArray f (0,1000000)
    f 0 = 1
    f n = n * dynFac (n-1)
