-- Ejercicio 1
-- Escribir ocho expresiones que denoten al número 8, diferentes a las ya vistas (4, 2+2, 3+1, doble
-- 2, doble (1+1)). Al menos seis deben usar funciones, de las cuales: al menos dos deben usar una
-- expresión lambda, tres deben usar doble y una debe usar cuadruple.

cuadruple :: Int -> Int
cuadruple x = 4 * x

doble :: Int -> Int
doble x = x + x

ocho0 :: Int
ocho0 = (\_ -> 8) 'H'

ocho1 :: Int
ocho1 = doble (succ 3) 

ocho2 :: Int 
ocho2 = doble (doble 2)

ocho3 :: Int 
ocho3 = (\x -> doble 4) "asd"

ocho4 :: Int 
ocho4 = cuadruple 2

ocho5 :: Int 
ocho5 = (\x -> succ x) 7

ocho6 :: Int 
ocho6 = (+) 3 (succ (doble (const 2 True))) 

ocho7 :: Int 
ocho7 = const 8 8

ocho8 :: Int 
ocho8 = id 8


-- Ejercicio 2
-- Mostrar que la expresión doble (doble 2) puede reducirse de otras formas que la vista en clase.

-- doble (doble 2)
-- =				(def doble)
-- doble (2 + 2)
-- =				(aritmetica)
-- doble 4		
-- =				(def doble)
-- 4 + 4
-- =				(aritmetica)
-- 8

-- doble (doble 2)
-- =				(def doble)
-- doble 2 + doble 2
-- =				(def doble)
-- 2 + 2 + doble 2
-- =				(def doble)
-- 2 + 2 + 2 + 2
-- =				(aritmetica)
-- 8

-- Ejercicio 3
-- Reducir cuadruple 2, y cuadruple (cuadruple 2). ¿Alguna de ellas puede hacerse de más de una forma?

-- cuadruple 2
-- =			(def cuadruple)
-- 8

-- cuadruple (cuadruple 2)
-- =				(def cuadruple)
--  4 * (cuadruple 2)
-- =				(def cuadruple)
--  4 * 4 * 2
-- =				(aritmetica)
-- 32

-- cuadruple (cuadruple 2)
-- =				(def cuadruple)
-- cuadruple (4 * 2)
-- =				(aritmetica)
-- cuadruple 8
-- =				(def cuadruple)
-- 32

-- Ejercicio 4
-- Definir las funciones triple, succ, sumarDos. Comprobar que twice succ = sumarDos.

-- triple :: Int -> Int
triple x = 3 * x

-- succ :: Int -> Int
succ' x = x + 1 --  succ ya existe en prelude

-- sumarDos :: Int -> Int
sumarDos x = x + 2

--  twice :: (a -> a) -> (a -> a) 
--  El tipo que debes haber visto hasta el momento es con los parentesis, cuando una funcion devuelve otra.
--  twice :: (a -> a) -> a -> a
twice f = g
	where g x = f(f x)

--  twice succ' = sumarDos --  Estas funciones toman un numero por parametro y devuelven otro. A ese numero lo llamamos n.

--  (twice succ') n 
--  =				(def twice)
-- g n
--  =				(def g, f = succ', x = n)
--  succ' (succ' n)
--  =				(def succ')
--  succ n + 1
--  =				(def succ')
--  n + 1 + 1 
--  = 			(aritmetica)
--  n + 2

--  sumarDos n
--  =			(def sumarDos, x = n)
--  n + 2

--Ejercicio 5
--Dar tres ejemplos de pares de expresiones que sean equivalentes entre sí, pero no estén vinculadas
--por reducción (además de cuadruple y \x → x+x). Dos de ellas deben no contenter lambdas.

ej50 :: Int -> Int
ej50 x = 2 * doble x

ej51 ::  Int -> Int 
ej51 x = twice doble x 

ej52 :: Int -> Int 
ej52 x = (\y -> 2 * 2 * x) 2

--Ejercicio 6
--Realizar la reducción completa de ((twice twice) doble) 3.
--twice f = g
--	where g x = f(f x)

-- ((twice twice) doble) 3
-- =					(def twice, f = twice)
-- (g doble) 3
-- =					(def g, x = doble)
-- (twice (twice doble)) 3
-- =					(def twice, f = twice doble)
-- g' 3
-- =					(def g, x = 3)
-- (twice doble) ((twice doble) x)
-- = 					(def twice)
-- (twice doble) (g'' x)
-- =					(def g'')
-- (twice doble) (doble (doble 3))
-- =					(def doble)
-- (twice doble) (doble (3 + 3))
-- = 					(aritmetica)
-- (twice doble) (doble 6)
-- =					(def doble)
-- (twice doble) 12
-- =					(def twice, f = doble)
-- g''' 12
-- =					(def g, x = 12)
-- doble (doble 12)
-- =					(def doble)
-- doble (12 + 12)
-- =					(aritmetica)
-- doble 24
-- =					(def doble)
-- 24 + 24
-- =					(aritmetica)
-- 48 


-- Ejercicio 7
-- Dar expresiones lambda que sean equivalentes a las siguientes expresiones:

-- triple
triple2 = \x -> x * 3

-- succ
succ2 = \x -> x + 1

-- sumarDos
sumarDos2 = \x -> succ2 (succ2 x)

-- twice
twice2 = \f -> \x -> f (f x)

-- twice twice
twiceTwice2 = \f -> \x -> f(f (f (f x)))


-- Ejercicio 8
-- Reescribir las siguientes funciones sin el uso de let, where o if-then-else cuando sea posible.

-- f x = let (y,z) = (x,x) in y -- El let se entide como "ejecuta lo de la derecha" y hacele pattern con lo de la izquierda.
f' x = x -- id
 
--greaterThan (x,y) = if x > y then True else False
greaterThan (x,y) = x > y

--f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
f'' x = x -- idem ejercicio 1 -- Idem id