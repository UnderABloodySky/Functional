--(f . swap) x y <<< : zipWith (f . swap)​ xs' ys'


sumaRara :: (Int, Int) -> Int -> Int
sumaRara (x,y) n = x + y + n

swap :: (a, b) -> (b, a)
swap (x,y) = (y, x)

example xs ys = zipWith (sumaRara.swap) xs ys