
instance Fractional Int where
  (/) = div

uno :: Int
uno = 1

sumaConUno :: Int -> Int
sumaConUno x = x + 1

suma :: [Int] -> Int
suma xs = 0

-- []
sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (x:xs) = (x + sumarLista xs)

promedio :: [Int] -> Int
promedio xs = (sumarLista xs) / (length xs)

   -- / (length xs)
