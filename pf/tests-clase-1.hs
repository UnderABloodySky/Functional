
sumaUno :: Int -> Int
sumaUno x = x + 1

-- mapi :: (a -> a) -> [a] -> [a]
-- mapi f [] = []
-- mapi f x:xs = f x : map' f xs

nats :: Int -> [Int]
nats x = [1..x]

take' :: Int -> [Int] -> [Int]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x : take' (n - 1) xs

isOne :: Int -> Bool
isOne 1 = True
isOne _ = False

map' :: (a -> a) -> [a] -> [a]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' f [] = []
filter' f (x:xs) = if not (isOne x)
                   then x : filter' f xs
                   else filter' f xs
