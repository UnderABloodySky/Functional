-- class Monad' m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- liftM :: Monad' m => (a -> b) -> m a -> m b
-- liftM f (return x) =

-- 2) Maybe

-- instance Monad Maybe where
--   return = Just
--   Nothing >>= f = Nothing
--   (Just x) >>= f = f x

data Nat = Z | S Nat deriving (Show, Eq)

pred3 x =
  do n <- pred x
     m <- pred n
     o <- pred m
     return o
  where pred Z = Nothing
        pred (S x) = Just x

pred3' x =
  pred x >>= \n ->
  pred n >>= \m ->
  pred m >>= \o ->
  return o
  where pred Z = Nothing
        pred (S x) = Just x

-- 3) List

-- perms xs =
--   if null xs then
--     return []
--   else do
--     x <- xs
--     xs' <- perms (xs // [x])
--     return (x : xs')
--
-- perms' xs =
--   if null xs then
--     return []
--   else xs >>= \x ->
--        perms' (xs // [x]) >>= \xs' ->
--        return (x:xs')


-- 4) Reader

-- newtype Reader k a = Reader (k -> a)

-- instance Monad (Reader k Int) where
--   return f = 0
--   n >>= f = f n

twice = do {x <- id; y <- (.x); return y}

twice' = id >>= \x ->
         (.x) >>= \y ->
         return y

-- 6) State

-- newtype State s a = ST (s -> (s, a))
--
-- instance Monad (State s) where
--   return x = ST (\s -> (s, x))
--   (ST f) >>= g = ST (\s ->
--                     let (s', x) = f s
--                         (ST h) = g x
--                     in h s')

-- import System.IO

main :: IO()

main =
  do
    text <- getLine
    if text /= "exit"
    then do
      appendFile "output.txt" (text ++ "\n")
      main
    else do
      content <- readFile "output.txt"
      putStrLn content
      return ()


-- ?) Test

newtype Trivial a = Tr a deriving Show

instance Functor Trivial where
  fmap f (Tr x) = Tr (f x)

instance Applicative Trivial where
  pure = Tr
  (<*>) (Tr f) = fmap f

instance Monad Trivial where
  return x = Tr x
  Tr x >>= f = f x

-- ?!?!?!??!?!?!?!?!??! .......
