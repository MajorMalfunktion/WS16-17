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
fresh = T $ \n -> (n,n+1)

--11.1.2
-- in do-Notation
numberTree :: Bintree a -> Trans Int (Bintree Int)
numberTree (Leaf _) = do
  x <- fresh
  return $ Leaf x
numberTree (Branch _ t1 t2) = do
    x <- fresh
    t1 <- numberTree t1
    t2 <- numberTree t2
    return $ Branch x t1 t2

-- in >>=-Notation
numberTreeB :: Bintree a -> Trans Int (Bintree Int)
numberTreeB (Leaf _)         = fresh >>= (\x -> return $ Leaf x)
numberTreeB (Branch _ t1 t2) = 
  fresh >>= (\x -> numberTreeB t1 >>=
    (\t1' -> numberTreeB t2 >>= (\t2' -> return (Branch x t1' t2'))))

-- naiv
numberTreeN :: Bintree a -> Trans Int (Bintree Int)
numberTreeN (Leaf _)         = T $ \st -> (Leaf st, st+1)
numberTreeN (Branch _ t1 t2) = T $ \st ->
  let (r1, st') = runT (numberTreeN t1) (st+1)
  in let (r2, st'') = runT (numberTree t2) st' in (Branch st r1 r2, st'')

-- Lösung mit manueller Zustandsübergabe
numberT :: Bintree a -> Int -> (Bintree Int,Int)
numberT (Leaf _) n = (Leaf n,n+1)
numberT (Branch _ t1 t2) n  = 
  let (t1',n' ) = numberT t1 (n+1)
      (t2',n'') = numberT t2 n'
  in (Branch n t1' t2',n'')
