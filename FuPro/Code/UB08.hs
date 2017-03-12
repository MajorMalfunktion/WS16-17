module UB08 where
import Vlfun

--a8.1

class Addition a where
    add :: a -> a -> a

instance Addition Nat where
    add Zero      x     = x
    add x         Zero  = x
    add (Succ x') x     = Succ (add x' x)

instance Addition PosNat where
    add One        x    = Succ' x
    add x          One  = x
    add (Succ' x') x    = Succ' (add x' x)

--a8.2

instance Eq Nat where
    Zero    == Zero     = True
    _       == _        = False
    Succ m  == Succ n   = m == n
