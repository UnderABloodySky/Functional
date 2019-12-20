data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Jamon | Queso | Aceitunas Int | Salsa | Anchoas | Cebolla deriving Show 

ejemploP :: Pizza
ejemploP = Capa (Aceitunas 4)(Capa Queso (Capa Queso (Capa Queso (Capa Queso (Capa Cebolla (Capa (Aceitunas 4) (Capa Jamon (Capa Salsa Prepizza)))))))) 

unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

-- Ejercicio 1) Definir las siguientes funciones utilizando recursión estructural explícita sobre ​ Pizza​ :
cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = unoSi (f i) + cantidadCapasQueCumplen f p

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p) 

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue f (Capa i p) = if f i
									then Capa i (soloLasCapasQue f p)
									else soloLasCapasQue f p


-- Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones
-- anteriores:
esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

sinLactosa :: Pizza -> Pizza
sinLactosa p = soloLasCapasQue (not . esQueso) p

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa  = (0 ==) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso =  cantidadCapasQueCumplen esQueso

dobleAceitunas :: Ingrediente -> Ingrediente
dobleAceitunas (Aceitunas n) = Aceitunas (2 * n) 
dobleAceitunas i = i

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas =  conCapasTransformadas dobleAceitunas


-- Ejercicio 3) Definir,
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
-- que expresa la definición de fold para la estructura de ​ Pizza​.
pizzaProcesada _ b Prepizza = b
pizzaProcesada f b (Capa i p) = f i (pizzaProcesada f b p)

recrPizza ::(Ingrediente -> Pizza -> b -> b) -> b -> Pizza -> b 
recrPizza _ b Prepizza = b
recrPizza f b (Capa i p) = f i p (recrPizza f b p)

-- Ejercicio 4) Resolver ttodas las funciones de los puntos 1) y 2) utilizando la función pizzaProcesada​ .
 
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' = \f -> pizzaProcesada​ (\i n -> unoSi (f i) + n) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' = \f -> pizzaProcesada​ (Capa . f) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' = \f -> pizzaProcesada​ (\i p -> if f i
													then Capa i p
													else p) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada​ (\i p -> if esQueso i
											then p
											else Capa i p) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa'  = pizzaProcesada​ ((&&).not.esQueso) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' =  pizzaProcesada​ (\i n -> unoSi (esQueso i) + n) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' =  pizzaProcesada​ (Capa . dobleAceitunas) Prepizza 

-- Ejercicio 5) Resolver las siguientes funciones utilizando ​ pizzaProcesada (si resulta
-- demasiado complejo resolverlas, dar primero una definición por recursión estructural
-- explícita, y usar la técnica la técnica de los “recuadros”):

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _ = 0

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada​ (\i n -> aceitunas i + n) 0
 
capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen = \f -> pizzaProcesada​ (\i l -> if f i
													then i:l
													else l) []

juntar :: Ingrediente -> Pizza -> Pizza
juntar (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntar i p = Capa i p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada​ juntar Prepizza

conCapasDe :: Pizza -> Pizza -> Pizza​
-- que agrega las capas de la primera pizza sobre la segunda
conCapasDe = flip(pizzaProcesada​ (\i p -> Capa i p)) 

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada​ (\i f n -> case n of
											0 -> Prepizza
											_ -> Capa i (f (n-1))) (const Prepizza))


-- Ejercicio 7) Definir las siguientes funciones de esquemas sobre listas, utilizando
-- recursión estructural de forma explícita:
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x
						then x : myFilter f xs 
						else myFilter f xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)

myRecr :: b -> (a -> [a] -> b -> b) -> [a] -> b
myRecr b _ [] = b
myRecr b f (x:xs) = f x xs (myRecr b f xs) 

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "Empty List"
myFoldr1 _ [x] = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

--(Desafío)​ 
-- myScanr :: (a -> b -> b) -> b -> [a] -> [b]

-- Ejercicio 9) Definir las siguientes funciones utilizando solamente ​ foldr​ :

sum' :: [Int] -> Int
sum' = foldr​ (+) 0

length' :: [a] -> Int
length' = foldr​ (\_ n -> succ n) 0

map' :: (a -> b) -> [a] -> [b]
map' = \f -> foldr​ ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' = \f -> foldr (\x xs -> if f x
									then x : xs
									else xs) []

find' :: (a -> Bool) -> [a] -> Maybe a
find' = \f -> foldr (\x r -> if f x
								then Just x
								else r) Nothing

any' :: (a -> Bool) -> [a] -> Bool
any' = \f -> foldr (\x b -> f x || b) False

all' :: (a -> Bool) -> [a] -> Bool
all' = \f -> foldr​ (\x b -> f x && b) True

countBy' :: (a -> Bool) -> [a] -> Int
countBy' = \f -> foldr​ (\x r -> if f x
									then succ r
									else r) 0
-- countBy' = \f -> foldr​ (\x n -> unoSi (f x) + n) 0

agregarPrimero x (xs, ys) =   (x:xs, ys)

agregarSegundo x (xs, ys) =   (xs, x:ys) 

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' = \f -> foldr​ (\x r -> if f x
										then agregarPrimero x r
										else agregarSegundo x r) ([], [])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = \f -> foldr​ (\x r ys -> case ys of
										[] -> []
										(y:ys') -> f x y : r ys') (const [])
-- scanr' :: (a -> b -> b) -> b -> [a] -> [b]


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = \f -> foldr​ (\x l -> if f x
									then x : l
									else []) []

take' :: Int -> [a] -> [a]
take' = flip (foldr​ (\x r n -> case n of
									0 -> []
									_ -> x : r (n-1) ) (const []))

drop' :: Int -> [a] -> [a]
drop' = flip (foldr (\x r n -> case n of
									0 -> x : r 0
									_ -> r (n-1)) (const []))
(!!!) :: Int -> [a] -> a
(!!!) = flip (foldr (\x r n -> case n of
									0 -> x
									_ -> r (n-1)) (error "Index too large"))

-- filter id :: [Bool] -> [Bool]
-- map (\x y z -> (x, y, z)) :: [a] -> [b -> c -> (a,b,c)]
-- map (+) :: [Int] -> [Int -> Int]
-- filter fst :: [(Bool, a)] -> [(Bool, a)]
-- filter (flip const (+)) :: [Bool] -> [Bool] 
-- map const :: [a] -> [b -> a]
-- map twice :: [a -> a] -> [a -> a]
-- foldr twice :: a -> [a -> a] -> a
-- zipWith fst :: [(a -> b, c)] -> [a] -> [b] 	
-- foldr (\x r z -> (x, z) : r z) (const []) :: [a] -> 	b -> [(a,b)]