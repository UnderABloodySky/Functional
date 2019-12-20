-- Clase Monadas


-- Primer nivel de tipos
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Alto orden

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) =
   if f x
      then x : filter' f xs
      else filter' f xs

-- Abstracción para algoritmos iguales

-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapMaybe : (a -> b) -> Maybe a -> Maybe b
-- fold :: (a -> b -> b) -> b -> [a] -> b
-- fmap :: (a -> b) -> f a -> f b

-- Polimorfismo ad-hoc

class functor f where
  fmap :: (a -> b) -> f a -> f b

instance functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

class Eq a where
  (==) :: a -> a -> Bool

longitud :: [Tree String] -> [Tree Int]
longitud = fmap (fmap length)

sonIguales :: Eq a => a -> [a] -> Bool
sonIguales y = foldr (\x r -> y == x && r) True

class Show a where
  Show :: a -> String

data Persona = P String Int deriving (Eq, Show)


-- foldMapa :: ([a] -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b

class foldable f where
  fold :: (a -> b -> b) -> b -> f a -> b

fmap id = id
fmap f . fmap g = fmap (f . g)


-- Magma a -> a -> a (cerrada en a)
-- |
-- V
--
-- Semigroup (asociativo)
-- |
-- V
--
-- Monoid (neutro)
-- |
-- V
--
-- Group (inverso)
--


-- Los monoides tienen neutro, cumplen la asociatividad
class Monoid m where
  (<>) :: a -> a -> a
  empty :: a

instance Monoid Int where
  (<>) = (+)
  empty = 0

instance [a] where
  (<>) = (++)
  empty = []

sum :: [Int] -> Int
concat :: [[a]] -> [a]

mconcat :: Monoid m => [m] -> m
mconcat = foldr (<>) empty

mconcat' :: (foldable f, Monoid m) => f m -> m
mconcat' = foldr (<>) empty


-- Manejo de errores

divM :: Int -> Int -> Maybe Int
divM x y = if y == 0
           then Nothing
           else Just (div x y)

recolectar :: [k] -> Map v k -> Maybe [v]
recolectar [x] m = case lookup x m of
                    Nothing -> Nothing
                    Just v -> Just [v]

recolectar (x:xs) m f a = case recolectar xs m of
                            Nothing -> Nothing
                            Just vs -> case lookup x m of
                              Nothing -> Nothing
                              Just v -> Just (v:vs)

recolectar :: [k] -> Map k v -> Maybe [v]
recolectar (x:xs) = recolectar x m >>= (\vs -> lookup x m >>= (\v -> Just (v:vs)))
recolectar [x] m = lookup x m >>= (\v -> Just [v])


(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) Nothing f = Nothing
(>>=) (Just x) f = f x

divM 5 (divM 6 (divM 7 8))
divM 7 8 >>= (\x -> divM 6 x >>= (\y divM 5 y))

recolectar :: [k] -> Map k v -> Maybe [v]
recolectar (x:xs) m = do
  vs <- recolectar x m bind
  v  <- lookup x m
  return (v:vs)


return :: a -> Maybe a
return = Just

divs = do
  x <- divM 7 8
  y <- divM 6 x
  z <- divM 5 y
  return z

(>>) :: Maybe a -> Maybe ()
(>>) m = m >> (\x -> return ())

existen :: [k] -> Map k v -> Bool
existen xs m = not (isNothing (foldr (>>) (return ()) (fmap (\x -> lookup x m ) xs)))

print :: Show a => a -> IO ()

readLn :: IO String

main :: IO ()
main = do
  s1 <- readLn
  s2 <- readLn
  s3 <- readLn
  print(mconcat [s1,s2,s3])
