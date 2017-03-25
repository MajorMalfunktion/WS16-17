module TransMonad where
import Control.Monad

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
