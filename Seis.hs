-- Ejercicio 1) 
-- Demostrar las siguientes propiedades:
compose f g x = f (g x)
cuadruple = \x -> 4 * x
doble x = x + x

-- Prop: doble = \x -> 2 * x
-- Dem: Por principio de extensionalidad:
--			V n :: Int . doble n = (\x -> 2 * x) n

-- doble n
-- =		(def doble)
-- n + n
-- =		(aritmetica)
-- 2 * n

-- (\x -> 2 * x) n
-- =			(BETA)
-- 2 * n

-- Prop: compose doble doble = cuadruple?
-- Dem: Por principio de extensionalidad:
-- 			V n :: Int . compose doble doble n = cuadruple n	

-- compose doble doble n
-- =			(def compose)
-- doble (doble n)
-- =			(def doble)
-- doble (n + n)
-- =			(def doble)
-- n + n + n + n
-- =			(aritmetica)
-- 4 * n

-- cuadruple n
-- =			(def cuadruple) 
-- (\x -> 4 * x) n 
-- =			(BETA)
-- 4 * n


-- Ejercicio 2)
-- Demostrar las siguientes propiedades:

x && y = not ((not x) || (not y))


-- not (x || y) = not x && not y

-- x = False
-- 		not (False || y)
-- 		=		(Identidad)
-- 		not y

-- not False && not y
-- =		(def not)
-- True && not y
-- =		(Identidad)
-- not y

-- x = True
-- 		not	(True || _) -- -- 
-- 		=		(Dominacion)
-- 		not True
-- 		=		(def not)
-- 		False

-- not True && not y
-- =			(def not)
-- False && not y 
-- =			(Dominacion)
-- False

-- Ejercicio 3)
-- Demostrar las siguientes propiedades:

suma' :: (Int, Int) -> Int
suma' (x,y) = x + y

suma :: Int -> Int -> Int
suma x y = x + y

-- Prop: curry suma' = suma
-- Dem: Por principio de extensionalidad:
--		V n :: Int V m :: Int . curry suma' n m = suma n m

-- suma n m
-- =		(def suma)
-- n + m

-- curry suma' n m
-- =		(def curry)
-- suma' (n,m)
-- =		(def suma')
-- n + m


-- Prop: uncurry suma = suma'
-- Dem: por principio de estensionalidad
--		V (x,y) :: (Int, Int) . uncurry suma (x,y) = suma' (x,y)

-- uncurry suma (x,y)
-- =				(def uncurry)
-- suma x y
-- =				(def suma)
-- x + y

-- suma' (x,y)
-- =		(def suma')
-- x + y


-- Ejercicio 4)
-- Demostrar las siguientes propiedades:

-- Prop: curry fst = const
-- Dem: Por principio de Estensionalidad
--			V x :: a . V y :: b . curry fst x y = const x y

-- curry fst x y
-- =			(def curry)
-- fst (x,y)
-- =			(def fst)
-- x

-- const x y
-- =			(def const)
-- x

-- Prop: uncurry (flip const) = snd
-- Dem: Por principio de Estensionalidad
--			V (x,y) :: (a, b). uncurry (flip const) (x,y) = snd (x,y) 

-- uncurry (flip const) (x,y)
-- =			(def uncurry)
-- flip const x y
-- =			(def flip)
-- const y x
-- =			(def const)
-- y

-- snd (x, y)
-- =			(def snd)
-- y


-- Ejercicio 5)
-- Demostrar las siguientes propiedades:

-- Prop: curry (uncurry f) = f
-- Dem: Por principio de Extensionalidad
--			V x :: a . V y :: b . curry (uncurry f) x y = f x y

-- curry (uncurry f) x y
-- =				(def curry)
-- uncurry f (x,y)
-- =				(def uncurry)
-- f x y

-- Prop: uncurry (curry f') = f'
-- Dem: Por principio de extensionalidad:
--			V (x,y) :: (a, b) . uncurry (curry f') = f' (x, y)

-- uncurry (curry f') (x,y)
-- = 				(def uncurry)
-- curry f' x y
-- =				(def curry)
-- f' (x, y) 


-- Ejercicio 6)
-- Dadas las siguientes definiciones
assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
appAssoc f p = f (assoc p)

-- demostrar la siguiente propiedad:
-- Prop: appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)
-- Dem: Por principio de Extensionalidad 
--			V (x,(y,z)) :: (a,(b,c)) . appAssoc (uncurry (uncurry f)) (x,(y,z)) = uncurry (compose uncurry f) (x,(y,z))

-- appAssoc (uncurry (uncurry f)) (x,(y,z)) 
-- =					(def appAssoc)
-- uncurry (uncurry f) (assoc (x,(y,z)))
-- =					(def assoc)
-- uncurry (uncurry f) ((x,y),z)
-- =					(def uncurry)
-- uncurry f (x,y) z
-- =					(def uncurry)
-- f x y z

-- uncurry (compose uncurry f) (x,(y,z))
-- =					(def uncurry)
-- compose uncurry f x (y, z)
-- =					(def compose)
-- uncurry (f x) (y, z)
-- =					(def uncurry)
-- f x y z


-- Ejercicio 7)
-- Dada la siguiente definición
-- (f . g) x = f (g x)

-- definir las siguientes funciones utilizando el operador ​ (.) y la menor cantidad de parámetros posible:
dupp x = (x,x)

cuadruple' = doble . doble 
doble' = suma'. dupp
twice' = \f -> uncurry (.) (dupp f)  
many' = \n f -> case n of
					0 -> id
					n -> f . many' (n-1) f

-- demostrar las siguientes propiedades:
-- f . g = compose f g
-- swap . swap = id
-- f . (g . h) = (f . g) . h
-- curry . uncurry = id
-- appAssoc f = f . assoc

compose f g x = f (g x)

-- Prop: f . g = compose f g
-- Dem: Por principio de Extensionalidad
--		V x :: c . (f .g) c = compose f g x

-- compose f g x
-- =			(def compose)
-- f (g x)

-- (f . g) x
-- =			(def (.))
-- f (g x)  

-- Prop: swap . swap = id
-- Dem: Por principio de extensionalidad
-- 		V (x,y) :: (a, b) . (swap . swap) (x,y) = id (x, y)

-- (swap . swap) (x,y)
-- =				(def (.))
-- swap (swap (x, y))
-- =				(def swap)
-- swap (y, x)
-- =				(def swap)
-- (x, y) 
-- =				(def id)
-- id (x, y)

-- Prop: f . (g . h) = (f . g) . h
-- Dem: Por principio de extencionalidad
--			V x :: a . (f . (g . h)) x = ((f . g) . h) x

-- ((f . g) . h) x
-- =				(def (.))
-- (f. g) (h x)
-- =				(def (.))
-- f (g (h x))

-- (f . (g . h)) x
-- =				(def (.))
-- f ((g . h) x)
-- =				(def (.))
-- f (g (h x))

-- Prop: curry . uncurry = id
-- Dem: Por principio de extensionalidad
-- 				V f :: (a -> b -> c) . V x :: a . V y :: b . (curry . uncurry) f x y = id f x y 

-- (curry . uncurry) f x y
-- =				(def (.))
-- curry (uncurry f) x y
-- =				(def curry)
-- uncurry f (x, y)
-- =				(def uncurry)
-- f x y
-- =				(def id)
-- id f x y


-- c. demostrar las siguientes propiedades sin utilizar el principio de extensionadoblelidad ni las definiciones de las funciones:

-- doble . doble = cuadruple
-- =					()
-- compose doble doble 
-- =					()
-- cuadruple

-- curry (uncurry (curry f)) = curry f
-- =					()
-- curry f

-- appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc
-- =					()
-- (uncurry (uncurry f)) . assoc
-- =					()
-- (uncurry . uncurry) f . assoc


-- (uncurry . uncurry) f . assoc = uncurry (uncurry . f)
-- =					()
-- uncurry uncurry f 
-- FALTA