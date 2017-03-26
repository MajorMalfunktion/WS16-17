module UB12 where
import Vlfun
import UB11

--A12.1

solutions, solutions' :: [(Int,Int,Int)]
solutions 
    =   do
        z <- [0..50]
        x <- [0..z]
        y <- [0..z]
        case (3*x^2 + 2*y +1 == z) of 
            True    -> return (x,y,z)
            False   -> []

solutions' 
    =   [0..50] >>= \z ->
        [0..z]  >>= \x ->
        [0..z]  >>= \y ->
        case (3*x^2 + 2*y +1 == z) of 
            True    -> return (x,y,z)
            False   -> []
            
--A12.2

f' :: Int -> Int -> Int -> Maybe Int
f' x y z 
    =   do
        fn   <- safeSqrt x
        fyz  <- safeDiv y z
        fm   <- safeSqrt fyz
        --fxyz <- safeDiv fn fm
        --return fxyz
        safeDiv fn fm

--A12.3

type Writer s a = (s,a)

tell :: s -> Writer s ()
tell s = (s,())

addW, mulW :: Int -> Int -> Writer String Int

addW x y = do
    tell $ show x ++ "+" ++ show y ++ "=" ++ show r ++"\n"
    return r
    where
    r = x+y

mulW x y = do
    tell $ show x ++ "+" ++ show y ++ "=" ++ show r ++"\n"
    return r
    where
    r = x*y

progW :: Writer String Int
progW = do
        e1 <- addW 3 1
        e2 <- mulW 2 e1
        addW e2 5

--A12.4
--
--type BStore x = x -> Bool
--
--bexp2store :: BExp x -> Store x -> Bstore x -> Bool
--bexp2store bexp st bst
--        = case bexp of
--            True_   -> True
--            False_  -> False
