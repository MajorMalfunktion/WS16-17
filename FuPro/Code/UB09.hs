module UB09 where
import Vlfun

--A9.1

preorderB, postorderB :: Bintree a -> [a]
preorderB tree
        = case tree of
            Empty       -> []
            Fork f l r  -> f : preorderB l ++ preorderB r

postorderB tree
        = case tree of
            Empty       -> []
            Fork f l r  -> postorderB l ++ postorderB r ++ [f]

--A9.2

--aus 
--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--(+) :: Num a, b, c => a -> b -> c
--
--ergibt sich
a92 :: Num a => [a] -> [a] -> [a]
a92 = zipWith (+)

--aus
-- f    :: a -> b
-- \x   :: a
--
-- f x  :: b
--
-- \f -> f x :: (a -> b) -> b
--
-- \x -> \f -> f x :: a -> (a -> b) -> b
b92 :: a -> (a -> b) -> b
b92 = \x -> \f -> f x

--a9.3

--1.
data Bank = Bank {konten :: [Konto]}
          deriving(Show) 

data Konto  = Konto {
        kontostand  :: Int,
        kunde       :: Kunde
}
            deriving(Show)

data Kunde  = Kunde {
        vorname :: String,
        name    :: String,
        addresse:: String
}
            deriving(Show)

--2.
bspBank1 :: Bank
bspBank1 = Bank {
        konten =[
        Konto{
                kontostand = 100,
                kunde = Kunde{
                        vorname = "k1",
                        name    = "K1",
                        addresse= "A1"
                }
        },
        Konto{
                kontostand = 200,
                kunde = Kunde{
                        vorname = "k2",
                        name    = "K2",
                        addresse= "A2"
                }
        }]
} 
-- does the exact same as bspBank1
bspBank2 ::Bank
bspBank2 = Bank [(Konto 100 (Kunde "k1" "K1" "A1")),(Konto 200 (Kunde "k2" "K2" "A2"))]

--3.
type ID = Int

credit :: Int -> ID -> Bank -> Bank
credit cred id (Bank kontos)
        = Bank{
        konten = updList konten id (kontos !! id){
                kontostand = (kontostand $ kontos !! id) + cred
        }
}

debit :: Int -> ID -> Bank -> Bank
debit deb 
        = credit (-deb)

transfer :: Int -> ID -> ID -> Bank -> Bank
transfer cred id0 id1
        = debit cred id0 . credit cred id1
