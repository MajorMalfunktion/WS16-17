module UB07 where
import Vlfun

--a7,1

data Person 
        = Person {  
                name :: String, 
                familyName :: String, 
                age :: Int  
        }
        deriving(Show, Read)

data NewPoint
        = NewPoint {
                x :: Double,
                y :: Double
        }

getX :: NewPoint -> Double
getX = x

setX :: Double -> NewPoint -> NewPoint
setX x' p = p{x = x'}


--a7.2

a72 :: Exp String
a72 = Sum [3 :* (Var "x" :^ 2), 2 :* Var "y", Con 1]

solutions :: [(Int,Int,Int)]
solutions 
        = [(x,y,z) | z <- [0..], x <- [0..z],  y <- [0..z], exp2store a72 (st x y) == z] 
                where
                st x y "x" = x
                st x y "y" = x

--7.3

simplify :: Exp x -> Exp x
-- Potenzen 
simplify (e :^ 0)   = Con 1
simplify (e :^ 1)   = simplify e
simplify (e :^ i)   = simplify e :^ i
-- Summen
simplify (Sum [])   = Con 0
simplify (Sum es)   = Sum $ filter (intEqCon 0) $ map simplify es
-- Produkte
simplify (Prod [])  = Con 1
simplify (Prod es)
        | any (intEqCon 0) list = Con 0
        | otherwise             = Prod $ filter (intEqCon 1) list
        where
        list = map simplify es
simplify (i :* e) = simpliMul (i :* simplify e)
        where
        simpliMul (0 :* _)      = Con 0 
        simpliMul (_ :* Con 0)  = Con 0
        simpliMul (1 :* e)      = e
        simpliMul (i :* Con 1)  = Con i
        simpliMul e             = e
-- nicht simplifizierbar
simplify e          = e
simplify (e :- e')  = simplify e :- simplify e'

--7.4

type BStore x = x -> Bool

bexp2store :: BExp x -> BStore x -> Store x -> Bool
bexp2store e bst st 
        = case e of 
            True_   -> True 
            False_  -> False
            BVar x  -> bst x
            Or es   -> or $ map f es
            And es  -> and $ map f es
            Not e   -> not $ f e
            e :<= e'-> exp2store e st <= exp2store e' st
            e := e' -> exp2store e st == exp2store e' st
        where
        f = (\x -> bexp2store x bst st)
