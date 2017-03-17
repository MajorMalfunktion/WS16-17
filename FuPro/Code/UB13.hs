module UB13 where
--import Vlfun
import Control.Monad

--A13.1
-- doesn't compile for whatever reason
--filtM :: MonadPlus m => (a -> Bool) -> [a] -> m a
--filtM _ []      = mzero
--filtM f (x:xs)  
--        | f x       = return mplus x $ filtM f xs
--        | otherwise = filtM f xs

--A13.2

type PointMethod = Trans (Double, Double)

getX, getY :: PointMethod Double
getX = T $ \(x,y) -> (x,(x,y))
getY = T $ \(x,y) -> (y,(x,y))

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
