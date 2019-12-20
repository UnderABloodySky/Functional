-- Ejercicio 1)

length' :: [a] -> Int
-- que describe la cantidad de elementos de la lista.
length' [] = 0
length' (x : xs) = 1 + length' xs

sum' :: [Int] -> Int​
-- que describe la suma de todos los elementos de la lista.
sum' [] = 0
sum' (n:ns) = n + sum' ns

product' :: [Int] -> Int​
-- que describe el producto entre todos los elementos de la lista.
product' [] = 1
product' (n:ns) = n * product' ns

concat' :: [[a]] -> [a]​
-- que describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concat' [] = []
concat' (xs:xss) = xs ++ concat xss

elem' :: Eq a => a -> [a] -> Bool​
-- que indica si el elemento dado pertenece a la lista.
elem' _ [] = False
elem' x (x':xs) = x == x' || elem' x xs 

all' :: (a -> Bool) -> [a] -> Bool​
-- que indica si todos los elementos de la lista cumplen el predicado dado.
all' _ [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
-- que indica si algún elemento de la lista cumple el predicado dado.
any' _ [] = False
any' f (x:xs) = f x || any' f xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

count :: (a -> Bool) -> [a] -> Int​
-- que describe la cantidad de elementos de la lista que cumplen el predicado dado.
count _ [] = 0
count f (x:xs) = unoSi (f x) + count f xs 

subset :: Eq a => [a] -> [a] -> Bool
-- que indica si todos los elementos de la primera lista se encuentran en la segunda.
subset [] _ = True
subset (x:xs) ys = elem x ys && subset xs ys

(+++) :: [a] -> [a] -> [a]​
-- que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
(+++) [] ys = ys
(+++) (x:xs) ys = x : (+++) xs ys

reverse' :: [a] -> [a]
-- que describe la lista que tiene los elementos en el orden inverso a la lista dada.
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]​
-- que describe la lista resultante de juntar de a pares los elementos de ambas listas, según la posición que comparten en cada una.
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

the :: ((a, b) -> c) -> [(a, b)] -> [c]
the _ [] = []
the f (p:ps) = f p : the f ps

firsts :: [(a, b)] -> [a]
firsts = the fst

seconds :: [(a, b)] -> [b]
seconds = the snd

unzip :: [(a,b)] -> ([a],[b])​
-- que describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se
-- corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes de dichos pares.
unzip ps = (firsts ps, seconds ps)

-- Ejercicio 2) Demostrar por inducción estructural las siguientes propiedades:

-- A)

-- Prop: length (xs ++ ys)​ ​ = ​ length xs + length ys
-- Dem: Por principio de induccion estructural en la estructura de xs (xs :: [a]).

-- Caso base: xs = []

-- length ([] ++ ys)
-- =				(def length.1)
--  length ys

-- length [] ++ length ys
-- =				(def length.1)
-- 0 + length ys
-- =				(aritmetica)
-- length ys

-- Caso inductivo: xs = x:xs'
--		HI: length (xs' ++ ys)​ ​ = ​ length xs' + length ys
--		TI: length ((x:xs') ++ ys)​ ​ = ​ length (x:xs') + length ys

-- length ((x:xs') ++ ys)
-- =				(def (++).2)
-- length (x :(xs' ++ ys))
-- =				(def length.2)
-- 1 + length (xs' ++ ys)
-- =				(HI)
-- 1 + length xs' + length ys
-- =				(Asociatividad)
-- (1 + length xs') + length ys
-- = 				(def length.2)
-- length (x:xs') + length ys

-- length (x:xs') + length ys
-- =				(def length.2)
-- 1 + length xs' + length ys
-- =				(HI)
-- 1 + length (xs' ++ ys)
-- =				(def length.2)
-- length (x : (xs' ++ ys))
-- =				(def length.2)
-- length ((x : xs') ++ ys)

-- B)
-- Prop: count (const True)​ ​ = ​ length
-- Dem: Por principio de Extensionalidad:
--			V xs :: [a] .  count (const True)​ ​ xs = ​ length xs
--		Por principio de induccion estructural en la estructura de xs. (xs :: [a])

-- Caso base: xs = []

-- count (const True)​ []
-- =				(def count.1)
-- 0

-- length []
-- =				(def length.1)
-- 0

-- Caso inductivo: xs = x : xs'
--			HI: count (const True)​ ​xs' = ​ length xs'
--			TI: count (const True)​ ​(x:xs') = ​ length (x:xs')

-- count (const True)​  (x:xs')
-- =				(def count.2)
-- unoSI (const True x) + count (const True)​ xs'
-- =				(def const)
-- unoSI True + count (const True)​ xs'
-- =				(def unoSi.1)
-- 1 + count (const True)​ xs'
-- =				(HI)
-- 1 + length xs'
-- =				(def length.2)
-- length (x:xs')

-- C)
-- Prop: elem ​ = ​ any . (==)
-- Dem: Por principio de extensionalidad.
--			V x :: a . elem x ​ = ​ any . (==) x
-- 		Por principio de extensionalidad:
--			V x :: a . xs :: [a] . elem x ​ xs = ​ (any . (==)) x xs
--		Por definicion del (.) es equivalente a demostrar:
--			V x :: a . xs :: [a] . elem x ​ xs = ​ any (==x)  xs

-- Caso base: xs = []  
-- 		elem x ​ [] = ​ any (==x)  []

-- elem x ​ [] 
-- =			(def elem.1)
-- False

-- any (==x)  []
-- =			(def any.1)
-- False

-- Caso Inductivo: xs = x' : xs'
--			HI: elem x ​ xs' = ​ any (==x)  xs'
-- 			TI: elem x ​ (x' : xs') = ​ any (==x)  (x' : xs')

-- any (==x)  (x' : xs')
-- =			(def any.2)
-- (==x) x' || any (==x) xs'
-- =			(Notacion)
-- x == x' || any (==x) xs'
-- =			(HI)
-- x == x' || elem x xs'
-- =			(def elem.2)
-- elem x (x':xs')


-- D)
-- any (elem x)​ ​ = ​ elem x . concat

-- E)
-- subset xs ys​ ​ = ​ all (flip elem ys) xs

-- F)
-- all null​ ​ = ​ null . concat

-- G)
-- length​ ​ = ​ length . reverse

-- H)
-- reverse (xs ++ ys)​ ​ = ​ reverse ys ++ reverse xs

-- I)
-- all p (xs ++ ys) ​ = ​ all p (reverse xs) && all p (reverse ys)

-- J)
-- No vale: unzip (zip xs ys)​ ​ = ​ (xs, ys)
-- Contraejemplo unzip (zip [0,1,2,3,4] [True, False, True, True])

data ExpA = Cte Int | Sum ExpA ExpA | Prod ExpA ExpA deriving Show

evalEA :: ExpA -> Int​
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA (Cte n) = n 
evalEA (Sum e1 e2) =  evalEA e1 + evalEA e2 
evalEA (Prod e1 e2) =  evalEA e1 * evalEA e2

simplificarEA :: ExpA -> ExpA​
-- que describe una expresión aritmética con el mismo significado que la dada, pero que no tiene
-- sumas del número 0, ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente ​ simbólica . ​
simplificarEA (Cte n) = Cte n 
simplificarEA (Sum e1 e2) = armarSum (simplificarEA e1) (simplificarEA e2) 
simplificarEA (Prod e1 e2) = armarProd (simplificarEA e1) (simplificarEA e2)

armarSum :: ExpA -> ExpA -> ExpA
armarSum (Cte 0) e2 = e2 	
armarSum e1 (Cte 0) = e1
armarSum e1 e2 = Sum e1 e2 

armarProd :: ExpA -> ExpA -> ExpA
armarProd (Cte 1) e2 = e2 
armarProd e1 (Cte 1) = e1
armarProd (Cte 0) _ = Cte 0 
armarProd _ (Cte 0) = Cte 0
armarProd e1 e2 = Prod e1 e2

esCero :: ExpA -> Int
esCero (Cte 0) = 1
esCero _ = 0

cantidadDeSumaCero :: ExpA -> Int​
-- que describe la cantidad de veces que aparece suma cero en la expresión aritmética dada. La
-- resolución debe ser exclusivamente ​ simbólica . ​
cantidadDeSumaCero (Cte n) = 0 
cantidadDeSumaCero (Sum e1 e2) = esCero e1 + esCero e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2   
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2 

ejemplo :: ExpA
ejemplo = Sum (Prod (Cte 1) (Sum (Cte 0)
								 (Cte 2))) 
			  (Sum (Prod (Cte 2)
			  			 (Sum (Cte 4)
			  			 	  (Cte 0)))
			  	   (Sum (Cte 2)
			  	   	    (Cte 0)))

-- Prop: evalEA . simplificarEA​ ​ = ​ evalEA
-- Dem: Por principio de extensionalidad:
-- 			V e :: ExpA . (evalEA . simplificarEA) e  = evalEA e
--		Por definicion de (.) es equivalente a demostrar:
--			V e :: ExpA . evalEA (simplificarEA e)  = evalEA e
--		Por principio de Induccion estructural en la estructura de e 

-- Caso base: e = Cte n
--			evalEA (simplificarEA (Cte n))  = evalEA (Cte n)

-- evalEA (simplificarEA (Cte n))			
-- =						(def simplificarEA.1)
-- evalEA (Cte n)

-- Caso Inductivo: e = Sum e1 e2
-- 			HI1: evalEA (simplificarEA e1  = evalEA e1
--			HI2: evalEA (simplificarEA e2  = evalEA e2  
--			TI:  evalEA (simplificarEA (Sum e1 e2))  = evalEA (Sum e1 e2)

-- evalEA (simplificarEA (Sum e1 e2))  
-- =						(def simplificarEA.2)
-- evalEA (armarSum (simplificarEA e1) (simplificarEA))
-- =						(lemaSuma)
-- evalEA (simplificarEA e1) + evalEA (simplificarEA e2)
-- =						(HI1)
-- evalEA e1 + evalEA (simplificarEA e2)
-- =						(HI2)
-- evalEA e1 + evalEA e2

-- evalEA (Sum e1 e2)
-- =						(def evalEA.2)
-- evalEA e1 + evalEA e2

-- Caso Inductivo: e = Prod e1 e2
--			evalEA (simplificarEA (Prod e1 e2))  = evalEA (Prod e1 e2)

-- Caso Inductivo: e = Prod e1 e2
-- 			HI1: evalEA (simplificarEA e1  = evalEA e1
--			HI2: evalEA (simplificarEA e2  = evalEA e2  
--			TI:  evalEA (simplificarEA (Prod e1 e2))  = evalEA (Prod e1 e2)

-- evalEA (simplificarEA (Prod e1 e2))  
-- =						(def simplificarEA.3)
-- evalEA (armarProd (simplificarEA e1) (simplificarEA))
-- =						(lemaProd)
-- evalEA (simplificarEA e1) * evalEA (simplificarEA e2)
-- =						(HI1)
-- evalEA e1 * evalEA (simplificarEA e2)
-- =						(HI2)
-- evalEA e1 * evalEA e2

-- evalEA (Prod e1 e2)
-- =						(def evalEA.3)
-- evalEA e1 * evalEA e2


-- lemaSuma
-- Prop: evalEA (armarSum e1 e2) = evalEA e1 + evalEA e2 
-- Dem: 

-- Caso e1 = Cte 0
-- 		evalEA (armarSum (Cte 0) e2) = evalEA (Cte 0) + evalEA e2

-- evalEA (armarSum (Cte 0) e2)
-- =						(def armarSum.1)
-- evalEA e2

-- evalEA (Cte 0) + evalEA e2
-- =						(def evalEA.1)
-- 0 + evalEA e2 
-- =						(Identidad)
-- evalEA e2

-- Caso e2 = Cte 0
-- 		evalEA (armarSum e1 (Cte 0)) = evalEA e1 + evalEA (Cte 0)

-- evalEA (armarSum e1 (Cte 0))
-- =						(def armarSum.2)
-- evalEA e1

-- evalEA e1 + evalEA (Cte 0)
-- =						(def evalEA.1)
-- evalEA e1 + 0 
-- =						(Identidad)
-- evalEA e1

-- Caso e1 != Cte 0 && e2 != Cte 0
-- 		evalEA (armarSum e1 e2) = evalEA e1 + evalEA e2

-- evalEA (armarSum e1 e2) 
-- =						(def armarSum.3)
-- evalEA (Sum e1 e2)
-- =						(def evalEA.2)
-- evalEA e1 + evalEA e2  	

-- lemaProd
-- Prop: evalEA (armarProd e1 e2) = evalEA e1 * evalEA e2 
-- Dem: 

-- Caso e1 = Cte 1
-- 		evalEA (armarProd (Cte 0) e2) = evalEA (Cte 0) * evalEA e2

-- evalEA (armarProd (Cte 1) e2)
-- =						(def armarProd.1)
-- evalEA e2

-- evalEA (Cte 1) * evalEA e2
-- =						(def evalEA.1)
-- 1 * evalEA e2 
-- =						(Identidad)
-- evalEA e2

-- Caso e2 = Cte 1
-- 		evalEA (armarProd e1 (Cte 1)) = evalEA e1 * evalEA (Cte 1)

-- evalEA (armarProd e1 (Cte 0))
-- =						(def armarSum.2)
-- evalEA e1

-- evalEA e1 + evalEA (Cte 0)
-- =						(def evalEA.1)
-- evalEA e1 + 0 
-- =						(Identidad)
-- evalEA e1


-- Caso e1 = Cte 0
-- 		evalEA (armarProd (Cte 0) e2) = evalEA (Cte 0) * evalEA e2

-- evalEA (armarProd (Cte 0) e2)
-- =						(def armarProd.3)
-- evalEA (Cte 0)
-- =						(def evalEA.1)
-- 0

-- evalEA (Cte 0) * evalEA e2
-- =						(def evalEA.1)
-- 0 * evalEA e2 
-- =						(Dominacion)
-- 0

-- Caso e2 = Cte 0
-- 		evalEA (armarProd e1 (Cte 0)) = evalEA e1 * evalEA (Cte 0)

-- evalEA (armarProd e1 (Cte 0))
-- =						(def armarSum.4)
-- evalEA (Cte 0)
-- =						(def evalEA.1)
-- 0

-- evalEA e1 * evalEA (Cte 0)
-- =						(def evalEA.1)
-- evalEA e1 * 0 
-- =						(Dominacion)
-- 0

-- Caso e1 != Cte 0 && e2 != Cte 0
-- 		evalEA (armarProd e1 e2) = evalEA e1 * evalEA e2
-- 
-- evalEA (armarProd e1 e2) 
-- =						(def armarProd.5)
-- evalEA (Prod e1 e2)
-- =						(def evalEA.2)
-- evalEA e1 * evalEA e2  	


-- Prop: cantidadSumaCero . simplificarEA​ ​ = ​ const 0

-- Dem: Por principio de Extensionalidad:
-- 			V e :: ExpA . (cantidadSumaCero . simplificardEA) e​ ​ = ​ const 0 e
--		Por definicion (.) 
-- 			V e :: ExpA . cantidadSumaCero (simplificardEA e​) ​ = ​ const 0 e

-- Por principio de Induccion estructural en la estructura de e (e :: ExpA)

-- Caso base: e = Ctn n
-- 			cantidadSumaCero (simplificarEA (Cte n)) ​ = ​ const 0 (Cte n)

-- cantidadSumaCero (simplificarEA (Cte n))
-- =						(def simplificarEA.1)
-- cantidadSumaCero (Cte n)
-- =						(def cantidadSumaCero.1)
-- 0

-- const 0 (Cte n)
-- =						(def const.1)
-- 0


-- Caso Inductivo: e = Sum e1 e2
--				HI1: cantidadSumaCero (simplificarEA e1​) ​ = ​ const 0 e1
--				HI2: cantidadSumaCero (simplificarEA e2​) ​ = ​ const 0 e2 
-- 				TI:  cantidadSumaCero (simplificarEA (Sum e1 e2)​) ​ = ​ const 0 (Sum e1 e2) 		 

-- cantidadSumaCero (simplificarEA (Sum e1 e2)​)
-- =										(simplificarEA.2) 
-- cantidadSumaCero (armarSum (simplificarEA e1) (simplificarEA e2))
-- =										(lemaCantSuma)
-- cantidadSumaCero (simplificar e1) + cantidadSumaCero (simplificar e2)
-- =										(HI)
-- const 0 e1  + cantidadSumaCero (simplificar e2)
-- =										(HI)
-- const 0 e1  + const 0 e2
-- = 										(def const)
-- 0  + const 0 e2    
-- = 										(def const)
-- 0 + 0 
-- =										(aritmetica)
-- 0

--​ const 0 (Sum e1 e2)
-- =						(def const)
-- 0

-- Caso Inductivo: e = Prod e1 e2
--				HI1: cantidadSumaCero (simplificarEA e1​) ​ = ​ const 0 e1
--				HI2: cantidadSumaCero (simplificarEA e2​) ​ = ​ const 0 e2 
-- 				TI:  cantidadSumaCero (simplificarEA (Prod e1 e2)​) ​ = ​ const 0 (Prod e1 e2) 		 

-- cantidadSumaCero (simplificarEA (Prod e1 e2)​)
-- =										(simplificarEA.1) 
-- cantidadSumaCero (armarProd (simplificarEA e1) (simplificarEA e2))
-- =										(lemaCantProd)
-- cantidadSumaCero (simplificar e1) + cantidadSumaCero (simplificar e2)
-- =										(HI)
-- const 0 e1  + cantidadSuma (simplificar e2)
-- =										(HI)
-- const 0 e1  + const 0 e2
-- = 										(def const)
-- 0  + const 0 e2    
-- = 										(def const)
-- 0 + 0 
-- =										(aritmetica)
-- 0

--​ const 0 (Prod e1 e2)
-- =						(def const)
-- 0


-- lemaCantSum
-- 	Prop: cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA e2)) =  cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 
-- 	Dem: 

-- Caso e1 = Cte 0
-- 		cantidadSumaCero (armarSuma (simplificarEA (Cte 0)) (simplificarEA e2)) = cantidadSumaCero (simplificarEA (Cte 0)) + cantidadSumaCero (simplificarEA e2) 

-- cantidadSumaCero (armarSuma (simplificarEA (Cte 0)) (simplificarEA e2)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarSuma (Cte 0) (simplificarEA e2))
-- =										(armarSuma.1)
-- cantidadSumaCero (simplificarEA e2)

-- cantidadSumaCero (simplificarEA (Cte 0)) + cantidadSumaCero (simplificarEA e2)
-- =										(simplificarEA.1)
-- cantidadSumaCero (Cte 0) + cantidadSumaCero (simplificarEA e2)
-- =										(def cantidadSumacero.1)
-- 0 + cantidadSumaCero (simplificarEA e2)
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e2)

-- Caso e2 = Cte 0
-- 		cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA (Cte 0)) = cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 0) 

-- cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA (Cte 0)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarSuma (simplificarEA e1) (Cte 0))
-- =										(armarSuma.1)
-- cantidadSumaCero (simplificarEA e1)

-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA (Cte 0)) 
-- =										(simplificarEA.1)
-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 0)
-- =										(def cantidadSumacero.1)
-- cantidadSumaCero (simplificarEA e1) + 0
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e1)

-- Caso e1 != Cte 0  &&  e2 != Cte 0  && e1 != Cte 1  &&  e2 != Cte 1
-- 		cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA e2) = cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 

-- cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA (Cte 0))
-- =										(def armarSuma.3)
-- cantidadSumaCero (Sum (simplificarEA e1) (simplificarEA e2))
-- =										(def cantidadSumaCero)
-- esCero (simplificarEA e1) + esCero (simplificarEA e2) + cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 
-- =										(otroLemaSuma)
-- <Problema Suma>

-- lemaCantProd
-- 	Prop: cantidadSumaCero (armarProd (simplificarEA e1) (simplificarEA e2)) =  cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 
-- 	Dem: 

-- Caso e1 = Cte 1
-- 		cantidadSumaCero (armarProd (simplificarEA (Cte 1)) (simplificarEA e2)) = cantidadSumaCero (simplificarEA (Cte 1)) + cantidadSumaCero (simplificarEA e2) 

-- cantidadSumaCero (armarProd (simplificarEA (Cte 1)) (simplificarEA e2)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarSuma (Cte 1) (simplificarEA e2))
-- =										(armarSuma.1)
-- cantidadSumaCero (simplificarEA e2)

-- cantidadSumaCero (simplificarEA (Cte 1)) + cantidadSumaCero (simplificarEA e2)
-- =										(simplificarEA.1)
-- cantidadSumaCero (Cte 1) + cantidadSumaCero (simplificarEA e2)
-- =										(def cantidadSumacero.1)
-- 0 + cantidadSumaCero (simplificarEA e2)
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e2)

-- Caso e2 = Cte 1
-- 		cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA (Cte 1)) = cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 1) 

-- cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA (Cte 0)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarSuma (simplificarEA e1) (Cte 0))
-- =										(armarSuma.1)
-- cantidadSumaCero (simplificarEA e1)

-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA (Cte 0)) 
-- =										(simplificarEA.1)
-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 0)
-- =										(def cantidadSumacero.1)
-- cantidadSumaCero (simplificarEA e1) + 0
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e1)


-- Caso e1 = Cte 0
-- 		cantidadSumaCero (armarProd (simplificarEA (Cte 0)) (simplificarEA e2)) = cantidadSumaCero (simplificarEA (Cte 0)) + cantidadSumaCero (simplificarEA e2) 

-- cantidadSumaCero (armarProd (simplificarEA (Cte 0)) (simplificarEA e2)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarProd (Cte 0) (simplificarEA e2))
-- =										(armarProd.3)
-- cantidadSumaCero (Cte 0)
-- =										(def cantidadSumaCero.1)
-- 0

-- cantidadSumaCero (simplificarEA (Cte 0)) + cantidadSumaCero (simplificarEA e2)
-- =										(simplificarEA.1)
-- cantidadSumaCero (Cte 0) + cantidadSumaCero (simplificarEA e2)
-- =										(def cantidadSumacero.1)
-- 0 + cantidadSumaCero (simplificarEA e2)
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e2)

-- Caso e2 = Cte 0
-- cantidadSumaCero (armarProd (simplificarEA e1) (simplificarEA (Cte 0)) = cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 0) 

-- cantidadSumaCero (armarProd (simplificarEA e1) (simplificarEA (Cte 0)) 
-- =										(def simplificarEA.1)
-- cantidadSumaCero (armarProd (simplificarEA e1) (Cte 0))
-- =										(armarProd.3)
-- cantidadSumaCero (Cte 0)
-- =										(def cantidadSumaCero.1)
-- 0

-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA (Cte 0)) 
-- =										(simplificarEA.1)
-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (Cte 0)
-- =										(def cantidadSumacero.1)
-- cantidadSumaCero (simplificarEA e1) + 0
-- =										(Identidad)
-- cantidadSumaCero (simplificarEA e1)
-- ACA PROBLEMA PROD

-- Caso e1 != Cte 0  &&  e2 != Cte 0 && e1 != Cte 1  &&  e2 != Cte 1
-- 		cantidadSumaCero (armarProd (simplificarEA e1) (simplificarEA e2) = cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 

-- cantidadSumaCero (armarSuma (simplificarEA e1) (simplificarEA e2)
-- =										(def armarProd.3)
-- cantidadSumaCero (Prod (simplificarEA e1) (simplificarEA e2))
-- =										(def cantidadSumaCero.3)
-- cantidadSumaCero (simplificarEA e1) + cantidadSumaCero (simplificarEA e2) 


-- Ejercicio 7) 
-- Dada la siguiente representación de expresiones aritméticas
data N = Z | S N deriving Show

evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

data ExpS = CteS N | SumS ExpS ExpS | ProdS ExpS ExpS deriving Show

evalES :: ExpS -> Int​
-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalES (CteS n) = evalN n
evalES (SumS e1 e2) = evalES e1 + evalES e2
evalES (ProdS e1 e2) = evalES e1 * evalES e2

es2ea :: ExpS -> ExpA
-- que describe una expresión aritmética representada con el tipo ExpA, que tiene el mismo significado que la dada.
es2ea (CteS n) = Cte (evalN n)
es2ea (SumS e1 e2) = Sum (es2ea e1) (es2ea e2)
es2ea (ProdS e1 e2) = Prod (es2ea e1) (es2ea e2)

ea2es :: ExpA -> ExpS
ea2es (Cte n) = CteS (int2N n)
ea2es (Sum e1 e2) = SumS (ea2es e1) (ea2es e2)
ea2es (Prod e1 e2) = ProdS (ea2es e1) (ea2es e2)

-- Prop: evalEA . es2ea​ ​ = ​ evalES
-- Dem: Por principio de extensionalidad: 
--				V e :: ExpS . (evalEA . es2ea) e = evalES e
--		Por definicion (.) 
--				V e :: ExpS . evalEA (es2ea e) = evalES e

--		Por principio de induccoion estructural:

-- Caso base: e = CteS n
--				evalEA (es2ea (CteS n)) = evalES (CteS n)				

-- evalEA (es2ea (CteS n))
-- =						(def es2ea.1)
-- evalEA (Cte (evalN n))
-- =						(def evalEA.1)
-- evalN n

-- evalES (CteS n)
-- =						(def evalES.1)
-- evalN n

-- Caso Inductivo: e = SumS e1 e2
--				HI1: evalEA (es2ea e1) = evalES e1
--				HI2: evalEA (es2ea e2) = evalES e2 
--				evalEA (es2ea (SumS e1 e2)) = evalES (SumS e1 e2)				

-- evalEA (es2ea (SumS e1 e2)) 
-- =						(def es2ea.2)
-- evalEA (Sum (es2ea e1) (es2ea e2))
-- =						(def evalEA.2)
-- evalEA (es2ea e1) + evalEA (es2ea e2)
-- =						(HI1)
-- evalES e1 + evalEA (es2ea e2)
-- =						(HI2)
-- evalES e1 + evalEA e2
-- =						(def evalES.2)
-- evalES (SumS e1 e2)


-- Caso Inductivo: e = ProdS e1 e2
--				HI1: evalEA (es2ea e1) = evalES e1
--				HI2: evalEA (es2ea e2) = evalES e2 
--				TI: evalEA (es2ea (ProdS e1 e2)) = evalES (ProdS e1 e2)				
 
-- evalEA (es2ea (ProdS e1 e2)) 
-- =						(def es2ea.3)
-- evalEA (Prod (es2ea e1) (es2ea e2))
-- =						(def evalEA.2)
-- evalEA (es2ea e1) * evalEA (es2ea e2)
-- =						(HI1)
-- evalES e1 * evalEA (es2ea e2)
-- =						(HI2)
-- evalES e1 * evalEA e2
-- =						(def evalES.2)
-- evalES (ProdS e1 e2)


-- Prop: es2ea . ea2es​ ​ = ​ id
-- Dem: Por principio de extensionalidad
--				V e :: ExpA . (es2ea . ea2es​) e ​ = ​ id e 
--      Por definicion de (.)
--				V e :: ExpA . es2ea (ea2es e) ​ = ​ id e 
-- 		Por principio de induccion estructural en la estructura de e:

-- Caso base: e = Cte n
--				es2ea (ea2es (Cte n)) ​ = ​ id (Cte n)

-- es2ea (ea2es  (Cte n)) 
-- =				(def ea2es.1)
-- es2ea (CteS (int2N n))
-- =				(def es2ea.1)
-- Cte (evalN (int2N n))
-- =				(def (.))
-- Cte ((evalN . int2N) n)
-- =				(lemita)
-- Cte n
-- =				(def id)
-- id (Cte n)

-- Caso Inductivo: e = Sum e1 e2
--				HI1: es2ea (ea2es e1) ​ = ​ id e1
--				HI2: es2ea (ea2es e2) ​ = ​ id e2
--				TI: es2ea (ea2es (Sum e1 e2)) ​ = ​ id (Sum e1 e2)

-- es2ea (ea2es (Sum e1 e2))
-- =						(def ea2es.2)
-- es2ea (Sums (ea2es e1) (ea2es e2))
-- =						(def es2ea.2)
-- Sum (es2ea (ea2es e1)) (es2ea (ea2es e2))
-- =						(HI1)
-- Sum (id e1) (es2ea (ea2es e2))
-- =						(HI2)
-- Sum (id e1) (id e2)
-- =						(def id)
-- Sum e1 (id e2)
-- =						(def id)
-- Sum e1 e2
-- =						(def id)
-- id (Sum e1 e2)

-- Caso Inductivo: e = Prod e1 e2
--				HI1: es2ea (ea2es e1) ​ = ​ id e1
--				HI2: es2ea (ea2es e2) ​ = ​ id e2
--				TI: es2ea (ea2es (Prod e1 e2)) ​ = ​ id (Prod e1 e2)

-- es2ea (ea2es (Prod e1 e2))
-- =						(def ea2es.3)
-- es2ea (ProdS (ea2es e1) (ea2es e2))
-- =						(def es2ea.3)
-- Prod (es2ea (ea2es e1)) (es2ea (ea2es e2))
-- =						(HI1)
-- Prod (id e1) (es2ea (ea2es e2))
-- =						(HI2)
-- Prod (id e1) (id e2)
-- =						(def id)
-- Prod e1 (id e2)
-- =						(def id)
-- Prod e1 e2
-- =						(def id)
-- id (Prod e1 e2)

-- lemita
-- Prop: (evalN . int2N) = id 
-- Dem: Por principio de extensionalidad.
--				V n :: Int . (evalN . int2N) n = id n
--  	Por definicion (.) 
--				V n :: Int . evalN (int2N n) = id n
--		Por principio de induccion estructural, en la estructura de n. (n :: Int)

-- Caso n == 0
-- 				evalN (int2N 0) = id 0
-- evalN (int2N 0)
-- =				(def int2N.1)
-- evalN Z
-- =				(def evalN.1)
-- 0
-- =				(def id)
-- id 0

-- Caso n > 0
--				HI: evalN (int2N (n -1)) = id (n - 1)
-- 				TI: evalN (int2N n) = id n
-- evalN (int2N n)
-- =				(def int2N.2)
-- evalN (S (int2N (n - 1)))
-- =				(def evalN.2)
-- 1 + evalN (int2N (n - 1))
-- =				(HI)
-- 1 + id (n - 1)
-- =				(def id)
-- 1 + (n - 1)
-- =				(Asociativibilidad)
-- 1 + n - 1
-- =					(aritmetica) 
-- n
-- =					(def id)
-- id n

-- Prop: ea2es . es2ea​ ​ = ​ id
-- Dem: Por principio de extensionalidad:
--			V e :: ExpS . (ea2es . es2ea​) e = id e
--		Por definicion del (.):
--			V e :: ExpS . ea2es (es2ea e) = id e
--		Por principio de induccion estructural en la estructura de e (e :: ExpS)

-- Caso base: e = CteS n
-- 		 ea2es (es2ea (CteS n)) = id (CteS n)

-- ea2es (es2ea (CteS n))
-- =					(def es2ea)
-- ea2es (Cte (evalN n))
-- =					(def ea2es)
-- CteS (int2N (evalN n))
-- =					(otroLemita)
-- CteS n
-- =					(def id)
-- id (CteS n)

-- Caso Inductivo: e = SumS e1 e2
--		 HI1: ea2es (es2ea e1) = id e1
--		 HI2: ea2es (es2ea e2) = id e2	
-- 		 TI:  ea2es (es2ea (SumS e1 e2)) = id (SumS e1 e2)

-- ea2es (es2ea (SumS e1 e2))
-- =						(def es2ea.2)
-- ea2es (Sum (es2ea e1) (es2ea e2))
-- =						(def ea2es.2)
-- SumS (ea2es (es2ea e1)) (ea2es (es2ea e2))
-- =						(HI1)
-- SumS (id e1) (ea2es (es2ea e2)) 
-- =						(HI2)
-- SumS (id e1) (id e2)
-- =						(def id)
-- SumS e1 (id e2)
-- =						(def id)
-- SumS e1 e2
-- =						(def id)
-- id (SumS e1 e2)  

-- Caso Inductivo: e = ProdS e1 e2
--		 HI1: ea2es (es2ea e1) = id e1
--		 HI2: ea2es (es2ea e2) = id e2	
-- 		 TI:  ea2es (es2ea (ProdS e1 e2)) = id (ProdS e1 e2)

-- ea2es (es2ea (ProdS e1 e2))
-- =						(def es2ea.3)
-- ea2es (Prod (es2ea e1) (es2ea e2))
-- =						(def ea2es.3)
-- ProdS (ea2es (es2ea e1)) (ea2es (es2ea e2))
-- =						(HI1)
-- ProdS (id e1) (ea2es (es2ea e2)) 
-- =						(HI2)
-- ProdS (id e1) (id e2)
-- =						(def id)
-- ProdS e1 (id e2)
-- =						(def id)
-- ProdS e1 e2
-- =						(def id)
-- id (ProdS e1 e2)  


-- otroLemita
-- Prop: (int2N. evalN) = id 
-- Dem: Por principio de extensionalidad.
--				V n :: N . (int2N. evalN) n = id n
--  	Por definicion (.) 
--				V n :: N . int2N (evalN n) = id n
--		Por principio de induccion estructural, en la estructura de n. (n :: N)

-- Caso base: n = Z
-- 				int2N (evalN Z) = id Z
-- int2N (evalN Z)
-- =				(def evalN.1)
-- int2N 0
-- =				(def int2N.1)
-- Z
-- =				(def id)
-- id Z

-- Caso Inductivo: n = S n'
--				HI: int2N (evalN n') = id n'
-- 				TI: int2N (evalN (S n')) = id (S n')

-- int2N (evalN (S n'))
-- =				(def evalN.2)
-- int2N (1 + evalN n')
-- =				(def int2N.2)
-- S (int2N (1 + evalN n' -1))
-- =				(aritmetica) 
-- S (int2N (evalN n'))
-- =				(HI)
-- S (id n')
-- =				(def id)
-- S n'
-- =				(def id)
-- id (S n') 
