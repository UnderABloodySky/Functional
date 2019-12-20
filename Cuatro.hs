-- Ejercicio 1) Determinar si las siguientes funciones son parciales o totales. Justificar.
-- Ejercicio 2) Para cada una de las funciones del ejercicio anterior, determinar si una o
-- más de las otras es equivalente a ella.

-- a - b - c
-- d - e

udiv (x,y) = div x y
-- Es parcial cuando y vale 0

udivE (x,0) = error “No puedo dividir por 0”
udivE (x,y) = div x y
-- Total 

udivH = uncurry div
--  Es parcial cuando y vale 0

succ x = x + 1
-- Total

succH = suma 1
-- Total

porLaMitad = flip div 2
-- Total

conDieresis ‘u’ = ‘ü’
--  Es parcial, dado que falla cuando el caracter evaluado es distinto a 'u'.

conDieresisB ‘u’ = ‘ü’
--  Es parcial, dado que falla cuando el caracter evaluado es distinto a 'u'.

conDieresisB c = conDieresis’ c
--  Es parcial o total dependiendo como sea conDieresis’.

conTildePM ‘a’ = ‘á’
conTildePM ‘e’ = ‘é’
conTildePM ‘i’ = ‘í’
conTildePM ‘o’ = ‘ó’
conTildePM ‘u’ = ‘ú’
--  Es parcial, dado que falla cuando se evalua un caracter distinto a las vocales en minusculas.

conTildeE c = if esVocal c
					then conTilde c
					else error “El valor recibido no es vocal”
-- Parcial

conTilde c = if esVocal c
					then conTilde c
					else c
-- Total


-- Ejercicio 3)
-- Dada la siguiente definición para la función ​ twice​ ,
twice0 = \f -> \x -> f (f x)
-- Determinar cuántos y cuáles son los redexes en las siguientes expresiones.

-- twice0 doble
-- =			(def twice0)
-- \f -> \x -> f (f x)

-- twice0 doble 2
-- =			(def twice0)
-- (\f -> \x -> f (f x)) doble 2
-- =			(BETA)
-- doble (doble 2)
-- =			(def doble)
-- doble (2 + 2)
-- =			(aritmetica)
-- doble 4
-- =			(def doble)
-- 4 + 4
-- =			(aritmetica)
-- 8

-- twice0
-- =			(def twice0)
-- (\f -> \x -> f (f x)) 


-- Ejercicio 4)
--  Dada la siguiente definición para la función ​ twice​ ,
twice1 f = g
	where g x = f (f x)

-- determinar cuántos y cuáles son los redexes en las siguientes expresiones.

-- twice1 doble
-- =			(def twice1)
-- g 			

-- twice1 doble 2
-- =			(def twice1)
-- g 2
-- =			(def g)
-- doble (doble 2)
-- = 			(def doble)
-- doble 2 + doble 2
-- =			(def doble)
-- 2 + 2 + doble 2
-- =			(def doble)
-- 2 + 2 + 2 + 2
-- =			(aritmetica)
-- 8

-- twice1
-- =			(def twice1)

-- Ejercicio 5)
-- Dada la siguiente definición para la función ​ twice​ ,
twice2 f x = f (f x)
-- determinar cuántos y cuáles son los redexes en las siguientes expresiones.

-- twice2 doble
-- =			(def twice3)

-- twice2 doble 2
-- =			(def twice2)
-- doble (doble 2)
-- =			(def doble)
-- doble (2 + 2)
-- = 			(aritmetica)
-- doble 4
-- =			(def doble)
-- 4 + 4
-- =			(aritmetica)
-- 8

-- twice2
-- =			(def twice2)


-- Ejercicio 6) En cada caso, intentar dar una expresión total y una parcial para cada
-- tipo a continuación. De no ser posible hacer alguno de los casos, explicar por qué.
-- No se puede dar expresiones de tipo tan generales, salvo Button