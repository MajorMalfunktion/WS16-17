module UB13 where
import Vlfun
import Control.Monad
import GHC.Unicode
import Data.Array

--A13.1
-- doesn't compile for whatever reason
--filtM :: MonadPlus m => (a -> Bool) -> [a] -> m a
--filtM _ []      = mzero
--filtM f (x:xs)  
--        | f x       = return mplus x $ filtM f xs
--        | otherwise = filtM f xs

--A13.2

-- runT :: (Double,Double) -> (a,(Double,Double))
type PointMethod = Trans (Double, Double)

-- PointMethod Double :=
-- runT :: (Double,Double) -> (Double,(Double,Double))
getX, getY :: PointMethod Double
getX = T $ \(x,y) -> (x,(x,y))
getY = T $ \(x,y) -> (y,(x,y))

-- PointMethod ():=
-- runT :: (Double,Double) -> ((),(Double,Double))
setX, setY :: Double -> PointMethod ()
setX x = T $ \(_,y) -> ((),(x,y))
setY y = T $ \(x,_) -> ((),(x,y))


--1.
pointSwap :: PointMethod ()
pointSwap = do
        x <- getX
        y <- getY
        setX y
        setY x

--2.
pointDistance :: (Double, Double) -> PointMethod Double
pointDistance (x',y')
    =   do
        x <- getX
        y <- getY
        return $ sqrt ((x' - x)^2 + (y' - y)^2)

--3.
prog :: PointMethod Double
prog = do
    x <- getX
    y <- getY
    pointSwap
    dist <- pointDistance(x,y)
    pointSwap
    return dist

--A13,3

main :: IO ()
main = do
    raw <- getLine
    writeFile "out.txt" (map toUpper raw)
    where
    

--A13.4

dynCatalan :: Int -> Int 
dynCatalan n = arr ! n
    where
    arr = mkArray (0,n) catalan
    catalan 0 = 1
    catalan n = sum $ map (\i -> arr ! i * arr ! (n-1-i)) [0..n-1]
