import Prelude hiding (not, and, or, ifThenElse, fst, snd, null, head, tail, length, (++), elem, reverse, zipWith)

-- Agrega al final
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x : (snoc xs y)

main = return ()

data Bool' = True' | False' deriving (Show, Eq)

-- 1) Booleanos

not :: Bool' -> Bool'
not True' = False'
not False' = True'

and :: Bool' -> Bool' -> Bool'
and True' True' = True'
and _ _ = False'

or :: Bool' -> Bool' -> Bool'
or False' False' = False'
or _ _ = True'

-- boolEq :: Bool' -> Bool' -> Bool'
-- boolEq x y = x == y

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t e = t
ifThenElse False t e = e

-- 2) Pares

fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,b) = b

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

tuppleEq :: (Eq a, Eq b) => (a,b) -> (a,b) -> Bool
tuppleEq (x,y) (z,w) = (x == z) && (y == w)

tuppleLessThan :: (Ord a, Ord b) => (a,b) -> (a,b) -> Bool
tuppleLessThan (x,y) (z,w) = (x <= z) && (y <= w)

-- 3) Maybe

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

liftMaybe :: a -> Maybe a
liftMaybe x = Just x

maybeEq :: Eq a => Maybe a -> Maybe a -> Bool
maybeEq Nothing Nothing = True
maybeEq (Just x) (Just y) = x == y
maybeEq _ _ = False

maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing = Nothing
maybeApply f (Just x) = Just (f x)

-- 4) Números naturales

data Nat = Zero | Suc Nat deriving (Show, Eq)

inc :: Nat -> Nat
inc Zero = Suc Zero
inc x = Suc x

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero (Suc Zero) = Suc Zero
add (Suc Zero) Zero = Suc Zero
add (Suc Zero) (Suc Zero) = Suc (Suc Zero)
add (Suc x) (Suc y) = Suc (Suc (add x y))

-- ***************************************

-- En demostraciones elegimos sobre qué parámetro vamos a aplicar el
-- principio de inducción

-- tail rec
-- add :: Nat -> Nat -> Nat
-- add (Suc n) m = add n (Suc m)
-- add Zero m = m

-- recursión estructural
-- add :: Nat -> Nat -> Nat
-- add (Suc n) m = Suc (add n m)
-- add Zero m = m

-- ***************************************

sub :: Nat -> Nat -> Maybe Nat
sub Zero Zero = Just Zero
sub Zero _ = Nothing
sub (Suc Zero) Zero = Just (Suc Zero)
sub (Suc x) (Suc Zero) = Just x
sub (Suc Zero) (Suc y) = sub Zero y
sub (Suc x) (Suc y) = sub x y

natEqual :: Nat -> Nat -> Bool
natEqual Zero Zero = True
natEqual Zero _ = False
natEqual _ Zero = False
natEqual (Suc x) (Suc y) = natEqual x y

natLessThan :: Nat -> Nat -> Bool
natLessThan Zero Zero = True
natLessThan Zero (Suc Zero) = True
natLessThan (Suc Zero) Zero = False
natLessThan (Suc x) (Suc y) = natLessThan x y

-- 5) Listas

null :: [a] -> Bool
null [] = True
null _ = False

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail [] = []
tail (x:xs) = xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
(++) [] [] = []
(++) xs [] = xs
(++) [] ys = ys
(++) xs (y:ys) = (++) (snoc xs y) ys

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (e:xs) = if x == e
                then True
                else elem x xs

indexOf :: [a] -> Int -> a
indexOf (x:xs) 0 = x
indexOf (x:xs) n = indexOf xs (n-1)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = snoc (reverse xs) x

-- 6) Patrones

-- a) si
-- b) si
-- c) no (parse error)
-- d) si
-- e) no (conflicting declaration)
-- f) no
-- g) si
-- h) si
-- i) no

-- 7) Reducción

-- a) add (Suc Zero) (Suc Zero)
--    (\x -> Suc x) (Suc Zero)

-- b) isNothing (sub Zero (Suc Zero))
--    (\x -> isNothing x) (sub Zero (Suc Zero))

-- c) fst (swap (True, False))
--    (\(x,y) -> x) (swap (True, False))

-- d) length [1,2,3]
--    (\(x:xs) -> 1 + length xs) [1,2,3] ?!?!?!?!?!?!!?

-- e) [1,2,3] !! 2
--    (\n -> (\(x:xs) -> (ifThenElse ((n-1) == 0) x xs))) 1 [1,2,3]

-- resta x veces el valor de y
sub' :: Nat -> Nat -> Maybe Nat
sub' Zero y = Just y
sub' _ Zero = Nothing
sub' (Suc x) (Suc y) = sub' x y


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] (y:ys) = []
zipWith f (x:xs) [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


-- co-recursión
fibs :: [Int]
fibs = 1:1:zipWith (+) fibs (tail fibs)


-- take 4 fibs
-- 1 : 1 : zipWith (+) fibs (tail fibs)
-- 1 : 1 : zipWith (+) (1:1:...) (1:...)
-- 1 : 1 : (1:1):zipWith (1:...) (...)

-- `pp` :: [a] -> Int -> a
-- `pp` (x:xs) 0 = x
-- `pp` (x:xs) n = `pp` xs (n-1)
