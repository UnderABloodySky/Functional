-- Descomentar para poder compilar ChurchBool and Church numerals
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ, concat, and, or, fst, snd, sum)

-------------------------------------------------------------

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f

type Nat = (Int -> Int)

toInt :: Nat -> Int
toInt n = n 0

zero :: Nat
zero = (+0)

one :: Nat
one = (+1)

succ :: Nat -> Nat
succ x = (\y -> x y + 1)

sum :: Nat -> Nat -> Nat
sum s1 s2 = s1 . s2

mult :: Nat -> Nat -> Nat
mult s1 s2 = applyN (toInt s1) s2
-- observacion:: applyN (toInt s1) s2 = applyN (toInt s2) s1

fromInt :: Int -> Nat
fromInt 0 = zero
fromInt n = succ (fromInt (n -1))

-------------------------------------------------------------

type Pair a b = forall c. (a -> b -> c) -> c

pair :: a -> b -> Pair a b
pair x y = (\p -> p x y)

fst :: Pair a b -> a
fst p = p (\x y -> x)

snd :: Pair a b -> b
snd p = p (\x y -> y)

swap :: Pair a b -> Pair b a
swap p = p (\x y -> pair y x)

-------------------------------------------------------------

type ChurchBool = forall a. a -> a -> a

true :: ChurchBool
true = (\t f -> t)

false :: ChurchBool
false = (\t f -> f)

ifelse :: ChurchBool -> a -> a -> a
ifelse p a b = p a b

and :: ChurchBool -> ChurchBool -> ChurchBool
and a b = a b false

or :: ChurchBool -> ChurchBool -> ChurchBool
or a b = a true b

not' :: ChurchBool -> ChurchBool
not' p = (\a b -> p b a)

xor :: ChurchBool -> ChurchBool -> ChurchBool
xor a b = a (not' b) b

toBool :: (Bool -> Bool -> a) -> a
toBool f = f True False

-- domM :: Map k v -> [k]
-- domM m = ??

-------------------------------------------------------------

type Set a = (a -> Bool)

-- no hace falta, pero para mantener consistencia con la interfaz
-- normal de conjuntos...
belongs :: Eq a => a -> Set a -> Bool
belongs x s = s x

-- acá sí hace falta
singleton :: Eq a => a -> Set a
singleton x = (\y -> x == y)
-- singleton = (==)

add :: Eq a => a -> Set a -> Set a
add x s = (\y -> x == y || s y)

union :: Set a -> Set a -> Set a
union s1 s2 = (\x -> s1 x || s2 x)

intersection :: Set a -> Set a -> Set a
intersection s1 s2 = (\x -> s1 x && s2 x)

complement :: Set a -> Set a
complement s = (\x -> not (s x))
-- complement = not . s

-- imposible
-- cardinal :: Set a -> Nat
-- cardinal s = ...

-- imposible
-- toList :: Set a -> [a]
-- toList s = ...

-------------------------------------------------------------

type Map k v = (k -> Maybe v)

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k m = m k

emptyM :: Map k v
emptyM = (\k -> Nothing)

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v m = (\k' -> if k == k' then Just v else m k')

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k m = (\k' -> if k == k' then Nothing else m k')

-------------------------------------------------------------

type DList a = ([a] -> [a])

toList :: DList a -> [a]
toList xs = xs []

nil :: DList a
nil = (\xs -> xs)
-- nil = id

fromList :: [a] -> DList a
fromList xs = (++ xs)

cons :: a -> DList a -> DList a
cons x xs = (\ys -> x : xs ys)

snoc :: DList a -> a -> DList a
snoc xs x  = (\ys -> xs (x : ys))

singletonL :: a -> DList a
singletonL x = (\ys -> x : ys)

append :: DList a -> DList a -> DList a
append xs ys = xs . ys

-- no se puede sin pasarlo a lista
head' :: DList a -> a
head' xs = head (toList xs)

-- no se puede sin pasarlo a lista
tail' :: DList a -> DList a
tail' xs = fromList (tail (toList xs))

-- observar cómo uso las operaciones de DList para armar la lista
concat :: [DList a] -> DList a
concat [] = nil
concat (x:xs) = append x (concat xs)

-------------------------------------------------------------

-- Sólo para valientes a modo de desafío
type ChurchNum = forall a. (a -> a) -> a -> a

zero' :: ChurchNum
zero' = (\f -> \x -> x)
-- zero' = flip const

one' :: ChurchNum
one' = (\f -> \x -> f x)
-- one' = ($)
-- one' = id

two' :: ChurchNum
two' = (\f -> \x -> f (f x))

-- bueno creo que se entendió
three' :: ChurchNum
three' = (\f -> \x -> f (f (f x)))

num' :: Int -> ChurchNum
num' n =  (\f -> \x -> applyN n f x)

succ' :: ChurchNum -> ChurchNum
succ' n = (\f -> \x -> f (n f x))

add' :: ChurchNum -> ChurchNum -> ChurchNum
add' n m = (\f -> \x -> m f (n f x))

mult' :: ChurchNum -> ChurchNum -> ChurchNum
mult' = (\m -> \n -> \f -> m (n f))

-- interesante no?
is_zero :: ChurchNum -> ChurchBool
is_zero = (\n -> n (\x -> false) true)

-- Desafío
-- pred' :: ChurchNum
-- pred' = ...

-- sub' :: ChurchNum -> ChurchNum -> ChurchNum
-- sub' = \m -> \n -> n pred m

unchurch_int :: ChurchNum -> Int
unchurch_int = (\a -> a (+1) 0)
