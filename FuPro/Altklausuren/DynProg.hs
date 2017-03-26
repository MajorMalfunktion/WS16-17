module DynProg where
import Data.Array
--class Ord a => Ix a where
--    range :: (a,a) -> [a]
--    index :: (a,a) -> a -> Int
--    inRange :: (a,a) -> a -> Bool
--    array :: Ix a => (a,a) -> [(a,b)] -> Array a b
--    (!) :: Ix a => Array a b -> a -> b
--    (//) :: Ix a => Array a b -> [(a,b)] -> Array a b
--    bounds :: Ix a => Array a b -> (a,a)
--
--    rangeSize :: (a,a) -> Int
--    rangeSize (a,b) = index (a, b) b+1

mkArray :: Ix a => (a -> b) -> (a,a) -> Array a b
mkArray f bounds = array bounds [(x,f x) | x <- range bounds]



instance Applicative (Trans state) where
    pure        = return  
--    T f <*> T x =

newtype Trans state a = T {runT :: state -> (a,state)}

instance Functor (Trans state) where
    fmap f (T h) = T $ (\(a,st) -> (f a,st)) . h

data Cotrans state a = (:#) {fun :: state -> a, state :: state}

instance Functor (Cotrans tate) where
    fmap f (h:#st) = (f . h):#st

instance Monad (Trans state) where
    return a    = T $ \st -> (a,st)
    T h >>= f   = T $ (\(a,st) -> runT (f a) st). h

c2wr :: Cotrans state a -> (state -> a,state)
c2wr (h:#st) = (h,st)

wr2c :: (state -> a,state) -> Cotrans state a
wr2c (h,st) = h:#st


