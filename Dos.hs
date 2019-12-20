-- Ejercicio 1)
-- Indicar los tipos de las siguientes definiciones:

first :: (a, b) -> a
first (x,y) = x

apply :: (a -> b) -> (a -> b)
apply f = g
	where g x = f x

twice :: (a -> a) -> (a -> a)
twice f = g
	where g x =f (f x)

doble :: Int -> Int
doble x = x + x

swap :: (a , b) -> (b, a)	
swap (x, y) = (y, x)

uflip :: ((a, b) -> c) -> ((b,a) -> c)
uflip f = g
	where g p = f (swap p)


-- Ejercicio 2) Dadas las definiciones anteriores, indicar el tipo de las siguientes
-- expresiones:


-- apply first
-- apply first :: (a,b) -> a

-- apply :: (a -> b) -> (a -> b)
-- first :: (c, d) -> c
-------------------------------------------- a <- (c,d)		~		b <- c
-- apply first :: (c, d) -> c


-- first (swap, uflip)
-- first (swap, uflip) ::  (a,b) -> a

-- first :: (a, b) -> a
-- swap :: (c, d) -> (d, c)
-- uflip :: ((d, e) -> f) -> ((e,d) -> f)
------------------------------------------- a <- (c, d) -> (d, c)		~		b <- ((d, e) -> f) -> ((e,d) -> f)   
-- first (swap, uflip) :: (c, d) -> (d, c)


-- twice doble
-- twice doble :: Int -> Int

-- twice :: (a -> a) -> (a -> a)
-- doble :: Int -> Int
------------------------------------------- a <- Int
-- twice doble :: Int -> Int


-- twice twice
-- twice twice :: (a -> a) -> (a -> a)

-- twice :: (a -> a) -> (a -> a)
-- twice :: (b -> b) -> (b -> b)
------------------------------------------ a <- (b -> b)  
-- twice twice :: (b -> b) -> (b -> b)


-- twice uflip  
-- twice uflip :: ((a, a) -> b) -> ((a,a) -> b)

-- twice :: (a -> a) -> (a -> a) 
-- uflip :: ((b, b) -> c) -> ((b,b) -> c) -- uflip se acopla al tipo de Twice
------------------------------------------ a <- ((b, c) -> d) -> ((c,b) -> d)  
-- twice uflip :: ((b, b) -> c) -> ((b,b) -> c)

-- twice uflip (\(x,y) -> x + y) (2, 4)
-- =					(def twice, f = uflip)
-- g (\(x,y) -> x + y) (2, 4)
-- =					(def g, x = (\(x,y) -> x + y))
-- uflip (uflip (\(x,y) -> x + y)) (2, 4)
-- = 					(def uflip, f =  (\(x,y) -> x + y))
-- g' (2, 4)
-- =					(def g', p = (2, 4))
-- uflip (\(x,y) -> x + y) (swap (2, 4))
-- =					(def swap)
-- uflip (\(x,y) -> x + y) (4, 2)
-- =					(def uflip)
-- g'' (4,2)
-- =					(def g'', p = (4,2))
-- (\(x,y) -> x + y) (swap (4,2))
-- =					(def swap)
-- (\(x,y) -> x + y) (2,4)
-- =					(Regla BETA) -- beta es el nombre de la regla que permite aplicar los lambdas.
-- 2 + 4
-- =					(aritmetica)
-- 6


-- twice swap
-- twice swap :: (a, a) -> (a, a)

-- twice :: (a -> a) -> (a -> a)  
-- swap :: (b, b) -> (b, b) 
------------------------------------------ a <- (b, b)  
-- twice swap :: (b, b) -> (b, b)


-- uflip swap
-- uflip swap :: (a, b) -> (a, b)

-- uflip :: ((a,b) -> c) -> (b,a) -> c
-- swap ::  (a, b) -> (b, a)
------------------------------------------ c <- (b, a)  
-- uflip swap :: (b, a) -> (b, a)


-- (twice twice) swap
-- (twice twice) swap :: (a',a') -> (a', a')

-- twice :: (a -> a) -> (a -> a)
-- twice :: (b -> b ) -> (b -> b ) 
------------------------------------------ a <- (b -> b)  
-- (twice twice) :: (b -> b) -> (b -> b)
-- swap :: (a',a') -> (a', a') 				   -- Swap se acopla al tipo de twice
------------------------------------------ b <- (a', a')  
-- (twice twice) swap :: (a',a') -> (a', a')


--Ejercicio 3) Dadas las siguientes definiciones y los siguientes tipos, asociar cada tipo
--con la función correspondiente.

const' :: a -> (b -> a) -- const existe en Prelude
const' x = g
	where g y = x

appDup :: ((a,a) -> b) -> (a -> b) 
appDup f = g
	where g x = f (x, x)

appFork :: (a ->  b, a -> c) -> (a -> (b, c)) 
appFork (f, g) = h
	where h x = (f x, g x)

appPar​ :: (a ->  b, c -> d) -> ((a, c) -> (b, d))
appPar​ (f, g) = h
	where h (x, y) = (f x , g y) 

appDist :: (a -> b) -> ((a,a) -> (b,b)) 
appDist f = g 
	where g (x, y) = (f x, f  y)

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f = h
	where h x = k
		where k y = (f y) x

subst :: (a -> b) -> (c -> (a -> b)) 
subst f = h
	where h g = k
		where k x = (f x)


-- Ejercicio 4) Para cada una de las siguientes expresiones decidir si poseen tipo. Si es
-- así indicar cuál es.

-- 1 && 2 == 2 
-- X

-- 1 + if 3 < 5 then 3 else 5 :: Int 

-- let par = (True, 4)
-- 	in (if first par 
-- 			then first par 
-- 			else second par)
-- X

-- (doble doble) 5
-- X

-- doble (doble 5) :: Int

-- twice first
-- X

-- (twice doble) doble
-- X

-- (twice twice) first
-- x

-- apply apply :: (a -> b) -> (a -> b) 


-- Ejercicio 5)
-- Dar dos ejemplos de expresiones que tengan cada uno de los siguientes tipos

-- Bool
ej0 = True
ej1 = not (const (8 == 4) 2)

-- (Int, Int)
ej2 = (8, 8)
ej3 = (doble 4, doble 4)

twoIfIsH :: Char -> Int
twoIfIsH 'H' = 2
twoIfIsH 'H' = 2
twoIfIsH _ = 0

-- Char -> Int
ej4 x = twoIfIsH x
ej5 x = const 2 x

isH :: Char -> Bool
isH 'H' = True
isH 'H' = True
isH _ = False

-- (Int, Char) -> Bool
ej6 (n, c) = isH c 
ej7 (n, c) = even n

-- (Int -> Int) -> Int
ej8 f = f 2 + f 2
ej9 f = twice f 2

-- (Bool -> Bool, Int)
ej10 = (not, 8)
ej11 = ((&& True), 4)

-- a -> Bool
ej12 x = False
ej13 x = const True x


-- Ejercicio 6) Para cada una de las siguientes expresiones, decir a cuál función del
-- ejercicio 3 es equivalente. Ofrecer argumentos de por qué son equivalentes.
-- Sobre todo, incluyendo que ademas de tener el tipo/hacer lo mismo, dos funciones son equivalentes, si al recibir el mismo elemento del conjunto de Entrada, devuelven el mismo elemento del conjunto de salida.
-- Estas funciones y las del ejercicio 3 respetan dicha propiedad.


-- \p -> let (f, g) = p in \x -> (f x, g x) = appFork :: (a ->  b, a -> c) -> (a -> (b, c)) 

-- \f -> (\x -> f (x, x)) = appDup :: ((a,a) -> b) -> (a -> b) 

-- \x -> (\y -> x) = const :: a -> (b -> a) 

-- \f -> (\px -> let (x, y) = px in (f x, f y)) = appDist :: (a -> b) -> ((a,a) -> (b,b)) 

-- \f -> (\x -> (\y -> (f y) x) = flip :: (a -> (b -> c)) -> (b -> (a -> c))

-- \pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y) = appPar​ :: (a ->  b, c -> d) -> ((a, c) -> (b, d))


-- Ejercicio 7) Encontrar cuales de estas expresiones son equivalentes entre sí.
-- Sugerencia: utilizar funciones anónimas es una forma interesante de encontrar
-- equivalencias entre expresiones que denotan funciones.

-- appFork (id,id)
-- \x -> appFork (id, id) x
-- \x -> (id x, id x)
-- \x -> (x, x)

-- appDup (appDist f)
-- \f -> appDup (appDist f)
-- \f -> (\x -> (appDist f) (x,x))
-- \f -> (\x -> (f x, f x)) 

-- appDup id
-- \x -> appDup id x
-- \x -> id (x, x)
-- \x -> (x, x)

-- appDup appFork
-- \f -> appFork (f, f)
-- \f -> (\x -> (f x, f x))


appDup :: ((a,a) -> b) -> (a -> b) 
appDup f = g
	where g x = f (x, x)

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f = h
	where h x = k
		where k y = (f y) x

-- flip (appDup const)
-- \x ->(\y -> (appDup const) y x) 
-- \x ->(\y -> (appDup const) y x) 
-- \x ->(\y -> const (y, y) x) 
-- \x ->(\y -> (y, y))

-- const' :: a -> (b -> a) -- const existe en Prelude
-- const' x = g
-- 		where g y = x

-- const (appDup id)
-- \x -> (appDup id)
-- \x -> (\y -> id (y, y))
-- \x -> (\y -> (y, y))


-- Ejercicio 8) En cada caso, intentar dar una expresión para cada tipo a continuación.
-- De no ser posible, explicar por qué.
-- Una expresion no puede dar tipo a 

cosaRara0 :: a
cosaRara0 = undefined

cosaRara1 :: Int -> a
cosaRara1 n = const (error "WTF 2.0") (n+n)

cosaRara2 :: a -> b
cosaRara2 _ = error "WTF 2.1"