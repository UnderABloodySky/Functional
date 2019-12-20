data Ingrediente = 	Jamon | Queso | Aceitunas Int | Salsa | Anchoas | Cebolla deriving Show
data Pizza = 	Prepizza | Capa Ingrediente Pizza  deriving Show

-- Ejercicio 1) 
-- Definir las siguientes funciones utilizando recursión estructural explícita sobre ​ Pizza​:

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0 
cantidadCapasQueCumplen f (Capa i pizza) = evaluate f i + (cantidadCapasQueCumplen f pizza)

evaluate :: (a -> Bool) -> a -> Int
evaluate f x = if f x
					then 1
					else 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i pizza) = Capa (f i) (conCapasTransformadas f pizza)


soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue f (Capa i pizza) = if f i
										then Capa i (soloLasCapasQue f pizza)
										else soloLasCapasQue f pizza

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False	

ejemplo :: Pizza
ejemplo = Capa (Aceitunas 8) (Capa Queso (Capa Queso (Capa (Aceitunas 2) (Capa (Aceitunas 4) (Capa Queso (Capa Queso (Capa Cebolla (Capa Queso (Capa Jamon (Capa Salsa (Capa Queso Prepizza)))))))))))


-- Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones anteriores:

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not.esQueso)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = \p -> 0 == cantidadCapasQueCumplen esQueso p 
-- o tambien: 
--aptaIntolerantesLactosa = (== 0) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int
--cantidadDeQueso = (\p -> cantidadCapasQueCumplen esQueso p)
-- O tambien:
cantidadDeQueso = cantidadCapasQueCumplen esQueso

dobleAceitunas :: Ingrediente -> Ingrediente
dobleAceitunas (Aceitunas n) = Aceitunas (n * 2)
dobleAceitunas i = i

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = \p -> conCapasTransformadas dobleAceitunas p 
-- O tambien:
--conElDobleDeAceitunas = conCapasTransformadas dobleAceitunas

--Ejercicio 3) Definir,
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
--que expresa la definición de fold para la estructura de ​ Pizza​ .
pizzaProcesada _ base Prepizza = base
pizzaProcesada f base (Capa i pizza) = f i (pizzaProcesada f base pizza)  

evaluate' :: Bool -> Int
evaluate' True = 1
evaluate' _ = 0

--Ejercicio 4) Resolver todas las funciones de los puntos 1) y 2) utilizando la función pizzaProcesada​ .
cantidadCapasQueCumplen1 :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen1 f = pizzaProcesada​ ((+) . evaluate f) 0
-- O tambien
--cantidadCapasQueCumplen' = \f -> pizzaProcesada​ ((+).evaluate'.f) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' = \f -> pizzaProcesada (Capa . f) Prepizza
-- O tambien
--conCapasTransformadas' = \f -> pizzaProcesada (\ingrediente pizza -> Capa (f ingrediente) pizza) Prepizza

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' = \f -> pizzaProcesada (armarPizzaSi f) Prepizza
-- O tambien:
-- soloLasCapasQue' = \f -> pizzaProcesada (\i p -> armarPizzaSi f i p) Prepizza

esCebolla :: Ingrediente -> Bool
esCebolla Cebolla = True
esCebolla _ = False

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

armarPizzaSi :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarPizzaSi  f i p = if f i 
							then Capa i p
							else p

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (armarPizzaSi (not.esQueso)) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i b -> not (esQueso i) && b) True
-- O tambien:
--aptaIntolerantesLactosa' = pizzaProcesada ((&&).not.esQueso) True

cantidadDeQueso' :: Pizza -> Int
--cantidadDeQueso' = pizzaProcesada (\ i n -> evaluate esQueso i + n) 0
-- O tambien:
cantidadDeQueso' = pizzaProcesada (\i n -> evaluate esQueso i + n) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . dobleAceitunas) Prepizza
-- conElDobleDeAceitunas' = pizzaProcesada (\i p -> Capa (dobleAceitunas i) p) Prepizza

-- Ejercicio 5) Resolver las siguientes funciones utilizando ​ pizzaProcesada

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n 
aceitunas _ = 0

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada ((+).aceitunas) 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen = \f -> pizzaProcesada (\i is -> case f i of
												 True -> i:is
												 False -> is) [] 

--O tambien:
--capasQueCumplen = \f -> pizzaProcesada (\i is -> if f i 
--													then i:is 
--													else is) [] 

juntar :: Ingrediente -> Pizza -> Pizza
juntar (Aceitunas n) (Capa (Aceitunas m) pizza) = Capa (Aceitunas (n+m)) pizza
juntar i pizza = Capa i pizza

conDescripcionMejorada :: Pizza -> Pizza
--conDescripcionMejorada = pizzaProcesada (\i p -> juntar i p) Prepizza
-- O Sino
conDescripcionMejorada = pizzaProcesada juntar Prepizza

conCapasDeExp :: Pizza -> Pizza -> Pizza
conCapasDeExp Prepizza p' = p'
conCapasDeExp (Capa i p) p' = Capa i (conCapasDeExp p p') 

conCapasDe :: Pizza -> Pizza -> Pizza​
conCapasDe = \p1 p2 -> pizzaProcesada​ (\i p -> Capa i p) p2 p1
-- O tambien:
--conCapasDe = flip (pizzaProcesada​ (\i p -> Capa i p))
-- Asi queda al reves: conCapasDe = pizzaProcesada​ (\i p -> Capa i p)
--conCapasDe = flip (pizzaProcesada​ Capa)

primerasNCapasExp :: Int -> Pizza -> Pizza
primerasNCapasExp 0 p1 = p1
primerasNCapasExp n (Capa i p) = Capa i (primerasNCapasExp (n-1) p) 


esCero :: Int -> Bool
esCero 0 = True
esCero _ = False

--primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas n = \p -> pizzaProcesada (\i f -> \m -> if m==0
--													then Prepizza
--													else Capa i (f (m-1))) (error "asd") p n   

--primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas n = \p -> pizzaProcesada (\i f -> \m -> if m==0
--													then Prepizza
--													else Capa i (f (m-1))) (\m -> Prepizza) p n   

--primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas n p = pizzaProcesada g h p n  
--	 where g i f 0 = Prepizza
--     g i f m = Capa i (f (m-1))
--       h m = Prepizza

--primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas = flip (pizzaProcesada (\i f -> \m -> if m==0
--													then Prepizza
--													else Capa i (f (m-1))) (error"pizza"))

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys
								-- myAppend' :: [a] -> [a] -> [a]t 
--myAppend' = foldr (\x r -> \ys -> x : r ys) id 
--myAppend' x s y s = foldr (\x r -> x : r) ys xs

--Funciones de listas con foldr
longitud :: [a] -> Int
longitud = foldr (\_ n -> 1 + n) 0

producto :: [Int] -> Int
--producto = foldr (\n m -> n * m) 1
producto = foldr (*) 1

concat' :: [[a]] -> [a]
--concat' = foldr (\xs ys -> xs ++ ys) []
concat' = foldr (++) []

all' :: (a -> Bool) -> [a] -> Bool
--all' = \f -> foldr (\x b -> f x && b) True
all' = \f -> foldr ((&&).f) True

map' :: (a -> b) -> [a] -> [b]
--map' = \f -> foldr (\x xs -> f x : xs) []
map' = \f -> foldr ((:).f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' = \f -> foldr (\x xs -> if f x 
									then x : xs
									else xs) []
reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

--Mismas funciones de listas pero con foldl
--foldl' :: (b -> a -> b) -> b -> [a] -> b
--foldl' f z [] = z
--foldl' f z (x:xs) = foldl' f (f z x) xs  

--longitud'' :: [a] -> Int
--longitud'' = foldl (\_ n -> succ n) 0  

--producto'' :: [Int] -> Int
--producto'' = foldr (\n m -> n * m) 1
--producto'' = foldr (*) 1

--concat'' :: [[a]] -> [a]
--concat'' = foldr (\xs ys -> xs ++ ys) []
--concat'' = foldr (++) []

--all'' :: (a -> Bool) -> [a] -> Bool
--all'' = \f -> foldr (\x b -> f x && b) True
--all'' = \f -> foldr ((&&).f) True

--map'' :: (a -> b) -> [a] -> [b]
--map'' = \f -> foldr (\x xs -> f x : xs) []
--map'' = \f -> foldr ((:).f) []

--filter'' :: (a -> Bool) -> [a] -> [a]
--filter'' = \f -> foldr (\x xs -> if f x 
--									then x : xs
--									else xs) []
--reverse'' :: [a] -> [a]
--reverse'' = foldr (\x xs -> xs ++ [x]) []

--producto'' :: [Int] -> Int
--producto'' = foldr (*) 1 


--Ejercicio 7) Definir las siguientes funciones de esquemas sobre listas, utilizando
--recursión estructural de forma explícita:

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x
							then x : myFilter f xs
							else myFilter f xs

--foldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

myTail :: [a] -> [a]
myTail = recr (error "Lista vacia") (\_ ls _ -> ls)

insert :: Ord a => a -> [a] -> [a]
insert = \x -> recr [x] (\a xs ys -> if x < a
										then x : a : xs
										else a : ys)

myInit :: [a] -> [a]
myInit = recr (error "Lista vacia") (\a ls zs -> if null ls
														then []
														else a : zs)

myMaximum :: Ord a => [a] -> a
myMaximum = recr (error "Lista vacia") (\i ls z -> if null ls
														then i
														else max i z)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [x] = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

myScanr :: (a -> b -> b) -> b -> [a] -> [b]  
myScanr _ z [] = [z]
myScanr f z (x:xs) =  f x (head (myScanr f z xs)) : myScanr f z xs  

--Explicitas para demostraciones:

capasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen'  _ Prepizza = []
capasQueCumplen'  f (Capa i p) = if f i 
									then i : capasQueCumplen f p
									else capasQueCumplen f p

cantidadDe :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadDe _ Prepizza = 0
cantidadDe f (Capa i pizza) = if f i
									then 1 + cantidadDe f pizza
									else cantidadDe f pizza

capasQueCumplen3 ::(Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen3 f Prepizza = []
capasQueCumplen3 f (Capa i p) = agregarSiCorresponde f i (capasQueCumplen3 f p)

agregarSiCorresponde::(Ingrediente -> Bool) -> Ingrediente -> [Ingrediente] -> [Ingrediente]
agregarSiCorresponde f i is = if (f i) then i:is else is
--capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
--capasQueCumplen _ Prepizza = []
--capasQueCumplen f (Capa i p) = juntarSi f i (capasQueCumplen f p)

--juntarSi :: (Ingrediente -> Bool) -> Ingrediente -> [Ingrediente]
--juntarSi f i xs = if f i
--						then i : xs
--						else xs

doble = \x -> 2*x


x = (succ.doble) 1 : [2,3,4]
many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = f . many (n - 1) f


--myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--myZipWith _ [] _ = []
--myZipWith _ _ [] = []
--myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys


--primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas n = \p -> pizzaProcesada (\i f -> \m -> if m==0
--													then Prepizza
--													else Capa i (f (m-1))) (\m -> Prepizza) p n   

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' =  \f -> foldr (\x r ys ->case ys of
												[] -> []
												(y:ys') ->  (f x y) : r ys') (\_ -> []) 

--Definir las siguientes funciones utilizando solamente ​ foldr​ :
sum' :: [Int] -> Int
sum' = foldr (+) 0

dropElem :: (a -> b) -> c -> a -> b
dropElem f x y = f y

length' :: [a] -> Int
length' = foldr​ (dropElem succ) 0
--length' = foldr​ (\_ ns -> 1 + ns) 0
--length' = foldr​ (\_ ns -> succ ns) 0

mapJ :: (a -> b) -> [a] -> [b]
mapJ = \f -> foldr ((:) . f) [] 
--mapJ = \f -> foldr (\x xs-> f x : xs) [] 

filterJ :: (a -> Bool) -> [a] -> [a]
filterJ = \f -> foldr (\x xs -> if f x
									then x : xs
									else xs) []

find' :: (a -> Bool) -> [a] -> Maybe a
find' = \f -> foldr (\x fr -> if f x
								then Just x
								else fr) Nothing

any' :: (a -> Bool) -> [a] -> Bool
any' = \f -> foldr (\x fr ->if f x
								then True
								else fr) False 

allJ :: (a -> Bool) -> [a] -> Bool
allJ = \f -> foldr ((&&) . f) True


countBy :: (a -> Bool) -> [a] -> Int
countBy = \f -> foldr (\x fr -> if f x
									then succ fr 
									else fr) 0

partitionExp :: (a -> Bool) -> [a] -> ([a], [a])
partitionExp _ [] = ([], [])
partitionExp f (x:xs) = if f x
							then agregarPrimero x (partitionExp f xs) 
							else agregarSegundo x (partitionExp f xs)

agregarPrimero :: a -> ([a], [a]) -> ([a], [a])
agregarPrimero x (xs, ys) = ((x:xs), ys)

agregarSegundo :: a -> ([a], [a]) -> ([a], [a])
agregarSegundo x (xs, ys) = (xs, (x:ys))

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = \f -> foldr (\x fr -> if f x
									then agregarPrimero x fr
									else agregarSegundo x fr) ([], [])

zipWithJ :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithJ = \f -> foldr (\x fr xs -> case xs of
										[] -> []
										(x':xs') -> f x x' : fr xs') (\_ -> [])

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f acc = foldr f' [acc]
					where f' b a = (f b (head a)) : a
     
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = \f -> foldr (\x fr -> if f x
									then x : fr
									else []) [] 

take :: Int -> [a] -> [a]
take = flip (foldr (\x fr m -> if m > 0
							then x : fr (m-1) 
							else []) (\_ -> []))

drop :: Int -> [a] -> [a]
drop =  flip (recr (const []) (\x xs r n-> if n == 0
												then x : xs
												else r (n-1)))

drop' :: Int -> [a] -> [a]
drop' = flip (foldr (\x r m -> if m == 0
									then x : r 0-- Por que  
									else r (m-1)) (const []))


(!!!) :: Int -> [a] -> a
(!!!) = flip(foldr (\x fr m -> if m == 0
							then x
							else fr (m-1)) (error "index too large"))


--curry :: ((a,b) -> c) -> a -> b -> c
--curry f x y = f (x,y)  
-- (,) = curry id
-- (,) 1
--fst = uncurry const
--snd = uncurry (flip const)
--curry fst = const