module Blatt11Loesung where

import Control.Applicative

-- Vorlage
newtype Trans state a = T { runT :: state -> (a,state) }

instance Functor (Trans state) where
  fmap g (T h) = T $ (\(a,st) -> (g a,st)) . h

instance Applicative (Trans state) where
  pure = return
  m <*> g = m >>= flip fmap g

instance Monad (Trans state) where
  return a = T $ \st -> (a,st)
  T trans >>= f = T $ \st -> let (a,st') = trans st in runT (f a) st'

data Bintree a = Leaf a | Branch a (Bintree a) (Bintree a) deriving Show

numbering :: Int -> Bintree a -> Bintree Int
numbering n t = fst $ runT (numberTree t) n

-- 11.1.1
fresh :: Trans Int Int
fresh = undefined

--11.1.2
numberTree :: Bintree a -> Trans Int (Bintree Int)
numberTree _ = undefined

-- Lösung mit manueller Zustandsübergabe
numberT :: Bintree a -> Int -> (Bintree Int,Int)
numberT (Leaf _) n = (Leaf n,n+1)
numberT (Branch _ t1 t2) n  = 
  let (t1',n' ) = numberT t1 (n+1)
      (t2',n'') = numberT t2 n'
  in (Branch n t1' t2',n'')

