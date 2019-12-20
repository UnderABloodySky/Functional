import Prelude hiding ((||))

(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True

($$) :: (a -> b) -> a -> b
-- ($$) f x = f x
-- ($$) f = \x -> f x
-- ($$) = \f -> \x -> f x
-- ($$) = \f -> f

-- ($$) x y = \x -> y
($$) = \f -> f

(¨) :: (b -> c) -> (a -> b) -> (a -> c)
-- (¨) f g x = f (g x)
(¨) = \f -> \g -> \x -> f (g x)

sumTuple :: (Int, Int) -> Int
sumTuple (x, y) = x + y

curry' :: ((a, b) -> c) -> a -> b -> c
-- curry' f x y = f (x, y)
curry' = \f -> \x -> \y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
-- uncurry' f (x, y) = f x y
uncurry' = \f -> \(x,y) -> f x y
