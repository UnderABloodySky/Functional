-- Ejercicio 1)
-- Dada la siguiente representación de expresiones aritméticas
data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

ejemploEA :: EA
ejemploEA = BOp Sum (BOp Mul (Const 4)
							  (BOp Sum  (Const 8) 
							  			(Const 4)))
					(BOp Sum (BOp Mul (Const 42) 
									  (Const 1))
							 (BOp Sum (Const 0)
							 		  (Const 2)))

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b)-> EA -> b
foldEA c b (Const n) = c n 
foldEA c b (BOp binop e1 e2) = b binop (foldEA c b e1)  (foldEA c b e2)

recrEA :: (Int -> b) -> (BinOp -> EA -> b -> EA -> b -> b) ->  EA -> b
recrEA c b (Const n) = c n
recrEA c b (BOp binop e1 e2) = b binop e1 (recrEA c b e1) e2 (recrEA c b e2)

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA  deriving Show

ejemploExpA :: ExpA
ejemploExpA = Suma (Prod (Cte 4)
							  (Suma  (Cte 8) 
							  		 (Cte 4)))
				   (Suma (Prod (Cte 42) 
							  (Cte 1))
					 	(Suma (Cte 0)
						      (Cte 2)))

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n) = c n 
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)

recrExpA :: (Int -> b) -> (ExpA -> b -> ExpA -> b -> b) -> (ExpA -> b -> ExpA -> b -> b) -> ExpA -> b
recrExpA c s p (Cte n) = c n
recrExpA c s p (Suma e1 e2) = s e1 (recrExpA c s p e1) e2 (recrExpA c s p e2)
recrExpA c s p (Prod e1 e2) = p e1 (recrExpA c s p e1) e2 (recrExpA c s p e2)

evalExpA :: ExpA -> Int
evalExpA (Cte n) = n 
evalExpA (Suma e1 e2) =  evalExpA e1 + evalExpA e2 
evalExpA (Prod e1 e2) =  evalExpA e1 * evalExpA e2

evalExpAf :: ExpA -> Int
evalExpAf = foldExpA id (+) (*) 

-- implementar las siguientes funciones:

evalEA :: EA -> Int​
-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA (Const n) = n
evalEA (BOp binop e1 e2) = evalBinOp binop (evalEA e1) (evalEA e2)

evalEAf :: EA -> Int​
evalEAf = foldEA id evalBinOp

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Sum = (+)
evalBinOp Mul = (*)

ea2ExpA :: EA -> ExpA
-- que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la dada.
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp binop e1 e2) = binOp2ExpA binop (ea2ExpA e1) (ea2ExpA e2) 

ea2ExpAf :: EA -> ExpA
ea2ExpAf = foldEA Cte binOp2ExpA

binOp2ExpA :: BinOp -> ExpA -> ExpA -> ExpA 
binOp2ExpA Sum = Suma
binOp2ExpA Mul = Prod

-- Prop: evalExpA . ea2ExpA ​ = ​ evalEA
-- Dem: Por princiopio de extencionalidad:
--			V e :: EA . (evalExpA . ea2ExpA) e = evalEA e
-- 		Por definicion del (.)
--			V e :: EA . evalExpA (ea2ExpA e) = evalEA e
-- 		Por princpio de induccion estructural en la estructura de e (e :: EA)

-- Caso base: Const n
-- 			evalExpA (ea2ExpA (Const n)) = evalEA (Const n)

-- evalExpA (ea2ExpA (Const n))
-- =							(ea2ExpA.1)
-- evalExpA (Cte n)
-- =							(def evalExpA.1)
-- n
-- =							(evalEA.1)
-- Const n

-- Caso inductivo: e = BOp binop e1 e2
--			HI1: evalExpA (ea2ExpA e1) = evalEA e1
--			HI2: evalExpA (ea2ExpA e2) = evalEA e2
--			TI:  evalExpA (ea2ExpA (BOp binOp e1 e2)) = evalEA (BOp binOp e1 e2)

-- evalExpA (ea2ExpA (BOp binop e1 e2))
-- =							(def ea2ExpA.2)
-- evalExpA (binOp2ExpA binop (ea2ExpA e1) (ea2ExpA e2)) 

-- Caso binop == Suma
-- 		evalExpA (binOp2ExpA Suma (ea2ExpA e1) (ea2ExpA e2)) = evalEA (BOp Sum e1 e2) 

-- evalExpA (binOp2ExpA Suma (ea2ExpA e1) (ea2ExpA e2)) 
-- =										(def binOp2ExpA.1)
-- evalExpA (Sum (ea2ExpA e1) (ea2ExpA e2))
-- =										(def evalExpA.2)
-- evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
-- = 										(HI1) 
-- evalEA e1 + evalExpA (ea2ExpA e2)
-- = 										(HI2) 
-- evalEA e1 + evalEA e2
-- =										(def evalEA.2)
-- evalEA (BOp Suma e1 e2)


-- Caso binop == Mult
-- 		evalExpA (binOp2ExpA Mult (ea2ExpA e1) (ea2ExpA e2)) = evalEA (BOp Mult e1 e2) 

-- evalExpA (binOp2ExpA Mult (ea2ExpA e1) (ea2ExpA e2)) 
-- =										(def binOp2ExpA.2)
-- evalExpA (Mult (ea2ExpA e1) (ea2ExpA e2))
-- =										(def evalExpA.3)
-- evalExpA (ea2ExpA e1) * evalExpA (ea2ExpA e2)
-- = 										(HI1) 
-- evalEA e1 * evalExpA (ea2ExpA e2)
-- = 										(HI2) 
-- evalEA e1 * evalEA e2
-- =										(def evalEA.3)
-- evalEA (BOp Mult e1 e2)


-- Ejercicio 2)
-- Dada la siguiente definición
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b) deriving Show
type Bolsa = [Objeto]
type Points = Int
data PJNumber = HP | MP  deriving Show
data Monstruo =  Gargola | Dragon | Duende Bolsa | Hada | Minotauro | Centauro  deriving Show
data Objeto = Piedra | Oro | Joya | Monedas Int | Escudo | Espada | Pocima PJNumber Points  deriving Show

ejemploArbolAB :: Arbol Monstruo Objeto
ejemploArbolAB = Nodo Hada (Nodo (Duende [Oro, Joya, Monedas 8]) (Hoja Espada)
								                                 (Hoja Escudo))
					  	   (Nodo Minotauro (Nodo Gargola  (Hoja (Pocima HP 40))
					  	   								  (Hoja Piedra))
					  	   				   (Nodo Centauro (Hoja (Monedas 16))
					  	   				   				  (Hoja (Pocima MP 20))))
--  que representa una estructura de árboles binarios;
-- implementar las siguientes funciones por recursión estructural:
cantidadDeHojas :: Arbol a b -> Int
-- que describe la cantidad de hojas en el árbol dado.
cantidadDeHojas (Hoja b) = 1
cantidadDeHojas (Nodo a a1 a2) = cantidadDeHojas a1 + cantidadDeHojas a2 

cantidadDeNodos :: Arbol a b -> Int
-- que describe la cantidad de nodos en el árbol dado.
cantidadDeNodos (Hoja b) = 0
cantidadDeNodos (Nodo a a1 a2) = 1 + cantidadDeNodos a1 + cantidadDeNodos a2 

cantidadDeConstructores :: Arbol a b -> Int​
-- describe la cantidad de constructores en el árbol dado.
cantidadDeConstructores (Hoja b) = 1
cantidadDeConstructores (Nodo a a1 a2) = 1 + cantidadDeConstructores a1 + cantidadDeConstructores a2

ea2Arbol :: EA -> Arbol BinOp Int​ 
-- que describe la representación como elemento del tipo ​ Arbol BinOp Int de la expresión aritmética dada.
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp binop e1 e2) = Nodo binop (ea2Arbol e1) (ea2Arbol e2)

foldArbol :: (b -> c) -> (a -> c -> c -> c)-> Arbol a b -> c
foldArbol h n (Hoja b) = h b
foldArbol h n (Nodo a a1 a2) = n a (foldArbol h n a1)  (foldArbol h n a2)

recrArbol :: (b -> c) -> (a -> Arbol a b -> c -> Arbol a b -> c -> c)-> Arbol a b -> c
recrArbol h n (Hoja b) = h b
recrArbol h n (Nodo a a1 a2) = n a a1 (recrArbol h n a1)  a2 (recrArbol h n a2)

cantidadDeHojas' :: Arbol a b -> Int
cantidadDeHojas' = foldArbol (const 1) (\_ n1 n2 -> n1 + n2)

cantidadDeNodos' :: Arbol a b -> Int
cantidadDeNodos' = foldArbol (const 0) (\_ n1 n2 -> succ n1 + n2)

cantidadDeConstructores' :: Arbol a b -> Int​
cantidadDeConstructores' = foldArbol (const 1) (\_ n1 n2 -> succ n1 + n2) 

ea2Arbol' :: EA -> Arbol BinOp Int​ 
ea2Arbol' = foldEA Hoja Nodo

-- demostrar la siguiente propiedad: cantidadDeHojas t + cantidadDeNodos t = ​ cantidadDeConstructores t
-- para todo ​ t ​ de tipo ​ Arbol a b
-- Prop: V t :: Arbol a b . cantidadDeHojas t + cantidadDeNodos t = ​ cantidadDeConstructores t
-- Dem: Por principio de induccion estructural en la estructura de t (t :: Arbol a b)

-- Caso base. t = Hoja b
-- 		cantidadDeHojas (Hoja b) + cantidadDeNodos (Hoja b) = ​ cantidadDeConstructores (Hoja b)

-- cantidadDeHojas (Hoja b) + cantidadDeNodos (Hoja b)
-- =								(def cantidadDeHojas.1)
-- 1 + cantidadDeNodos (Hoja b)
-- =								(def cantidadDeNodos.1)
-- 1 + 0 
-- =								(aritmetica)
-- 1
-- =								(def cantidadDeConstructores.1)
-- cantidadDeConstructores (Hoja b)

-- Caso base. t = Nodo a t1 t2
-- 		HI1: cantidadDeHojas t1 + cantidadDeNodos t1 = ​ cantidadDeConstructores t1
--		HI2: cantidadDeHojas t2 + cantidadDeNodos t2 = ​ cantidadDeConstructores t2
-- 		TI:  cantidadDeHojas (Nodo a t1 t2) + cantidadDeNodos (Nodo a t1 t2) = ​ cantidadDeConstructores (Nodo a t1 t2)

-- cantidadDeHojas (Nodo a t1 t2) + cantidadDeNodos (Nodo a t1 t2)
-- =								(def cantidadDeHojas.2)
-- cantidadDeHojas t1 + cantidadDeHojas t2  + cantidadDeNodos (Nodo a t1 t2)
-- =								(def cantidadDeNodos.2)
-- cantidadDeHojas t1 + cantidadDeHojas t2  + cantidadDeNodos t1 + cantidadDeNodos t2
-- =								(Conmutatividad)
-- cantidadDeHojas t1 + cantidadDeNodos t1 + cantidadDeNodos t2 + cantidadDeHojas t2
-- =								(Asoc, HI1)
-- cantidadDeContructores t1 + cantidadDeNodos t2 + cantidadDeHojas t2
-- =								(Asoc, HI1)
-- cantidadDeContructores t1 + cantidadDeContructores t2
-- =								(def cantidadDeContructores.2)
-- cantidadDeContructores (Nodo a t1 t2)

-- Ejercicio 3)
-- Dada la siguiente definición

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree z n EmptyT = z
foldTree z n (NodeT x t1 t2) = n x (foldTree z n t1) (foldTree z n t2)

recrTree :: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b
recrTree b n EmptyT = b
recrTree b n (NodeT a t1 t2) = n a t1 (recrTree b n t1) t2 (recrTree b n t2)

ejemploT :: Tree Int
ejemploT = NodeT 2 (NodeT 4 EmptyT 
	                       (NodeT 16 (NodeT 3 (NodeT 18 EmptyT
	                      	                           EmptyT)
	                      	                  EmptyT)
	                      	        (NodeT 42 EmptyT
	                      	        	      EmptyT)))
                 (NodeT 8 (NodeT 32 (NodeT 128 EmptyT
                 	                           EmptyT)
                 	                (NodeT 64 EmptyT 
                 	                	      (NodeT 256 EmptyT
                 	                	      	         (NodeT 512 EmptyT
                 	                	      	         	        EmptyT)))) 
                 	      (NodeT 1024 (NodeT 42 EmptyT
                 	      	                    EmptyT)
                 	      	          (NodeT 0 EmptyT
                 	      	          	       EmptyT)))
-- que expresa la estructura de árboles binarios;
-- implementar las siguientes funciones utilizando recursión estructural:

sumarT :: Tree Int -> Int
-- que describe el número resultante de sumar todos los números en el árbol dado.
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2
 
sumar3 :: Int -> Int -> Int -> Int
sumar3 x y z = x + y + z 

-- foldTree z n (NodeT x t1 t2) = n x (foldTree z n t1) (foldTree z n t2)
sumarT' :: Tree Int -> Int
sumarT' = foldTree 0 sumar3

sizeT :: Tree a -> Int
-- que describe la cantidad de elementos en el árbol dado.
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2    

sizeT' :: Tree a -> Int
sizeT' = foldTree 0 (\_ n1 n2 -> succ n1 + n2)

anyT :: (a -> Bool) -> Tree a -> Bool
-- que indica si en el árbol dado hay al menos un elemento que cumple con el predicado dado.
anyT _ EmptyT = False
anyT f (NodeT n t1 t2) = f n || anyT f t1 || anyT f t2 

anyT' :: (a -> Bool) -> Tree a -> Bool
anyT' = \f -> foldTree False (\x b1 b2 -> f x || b1 || b2)

unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

countT :: (a -> Bool) -> Tree a -> Int
-- que describe la cantidad de elementos en el árbol dado que cumplen con el predicado dado.
countT _ EmptyT = 0  
countT f (NodeT n t1 t2) = unoSi (f n) + countT f t1 + countT f t2  

countT' :: (a -> Bool) -> Tree a -> Int
countT' = \f -> foldTree 0 (sumar3 . unoSi . f)

countLeaves :: Tree a -> Int​
-- que describe la cantidad de hojas del árbol dado.
countLeaves EmptyT = 1  
countLeaves (NodeT n t1 t2) = countLeaves t1 + countLeaves t2  

countLeaves' :: Tree a -> Int​
countLeaves' = foldTree 1 (const (+)) 

heightT :: Tree a -> Int​
-- que describe la altura del árbol dado.
heightT EmptyT = 0 
heightT (NodeT n t1 t2) = 1 + max (heightT t1) (heightT t2) 

heightT' :: Tree a -> Int
heightT' = foldTree 0 ​(const (\n1 n2 -> succ (max n1 n2)))

inOrder :: Tree a -> [a]
-- que describe la lista ​ in order con los elementos del árbol dado.
inOrder EmptyT = [] 
inOrder (NodeT n t1 t2) = inOrder t1 ++ (n : []) ++ inOrder t2

inOrder' :: Tree a -> [a]
inOrder' = foldTree [] (\x l1 l2 -> l1 ++ (x : []) ++ l2)

merge :: [[a]] -> [[a]] -> [[a]]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

listPerLevel :: Tree a -> [[a]]
-- que describe la lista donde cada elemento es una lista con los elementos de un nivel del árbol dado.
listPerLevel EmptyT = [] 
listPerLevel (NodeT x t1 t2) = [x] : merge (listPerLevel t1) (listPerLevel t2)

listPerLevel' :: Tree a -> [[a]]
listPerLevel' = foldTree [] (\x l1 l2 -> [x] : merge l1 l2)

mirrorT :: Tree a -> Tree a​
-- que describe un árbol con los mismos elemento que el árbol dado pero en orden inverso.
mirrorT EmptyT = EmptyT  
mirrorT (NodeT n t1 t2) = NodeT n (mirrorT t2) (mirrorT t1) 

mirrorT' :: Tree a -> Tree a​
mirrorT' = foldTree EmptyT (flip . NodeT)

levelN :: Int -> Tree a -> [a]
-- que describe la lista con los elementos del nivel dado en el árbol dado.
levelN _ EmptyT = []
levelN n (NodeT x t1 t2) = case n of
								0 -> [x]
								_ -> levelN (n-1) t1 ++ levelN (n-1) t2  

levelN' :: Int -> Tree a -> [a]
levelN' = flip (foldTree (const []) (\x f1 f2 n -> if n == 0
												then [x]
							            		else f1 (n-1) ++ f2 (n-1)))

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga [] ys = ys
listaMasLarga xs [] = xs
listaMasLarga xs ys = if length xs > length ys
									then xs
									else ys 

ramaMasLargaP :: Tree a -> [a]
-- que describe la lista con los elementos de la rama más larga del árbol.
ramaMasLargaP EmptyT = [] 
ramaMasLargaP (NodeT x t1 t2) = x : listaMasLarga (ramaMasLargaP t1) (ramaMasLargaP t2)

pathLonger :: [a] -> [a] -> [a]
pathLonger xs ys = if length xs > length ys
								then xs
								else ys 

ramaMasLarga' :: Tree a -> [a]
ramaMasLarga' = foldTree [] (\x l1 l2 -> x : pathLonger l1 l2)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = [] 
agregarATodos x (xs:xss) = (xs ++ [x]) : agregarATodos x xss

isEmptyT :: Tree a -> Bool
isEmptyT EmptyT = True 
isEmptyT _ = False

todosLosCaminos :: Tree a -> [[a]]
-- que describe la lista con todos los caminos existentes en el árbol dado.
-- todosLosCaminos EmptyT = [[]] 
todosLosCaminos EmptyT = []
-- todosLosCaminos (NodeT x t1 t2) = map (x:) (todosLosCaminos t1 ++ todosLosCaminos  t2)
-- todosLosCaminos (NodeT n t1 t2) = agregarATodos n (todosLosCaminos t1 ++ todosLosCaminos t2)  
-- todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]] 
todosLosCaminos (NodeT x t1 t2) = if isEmptyT t1 && isEmptyT t2
												then [[x]]
												else map (x:) (todosLosCaminos t1 ++ todosLosCaminos  t2)

todosLosCaminos' :: Tree a -> [[a]]
todosLosCaminos' = foldTree [] (\x l1 l2 -> if null l1 && null l2
												then [[x]]
												else map (x:) (l1 ++ l2)) 

-- Prop: heightT ​ = ​ length . ramaMasLarga
-- Dem: Por principio de extensionalidad:
--				V t :: Tree a . heightT ​t = ​ (length . ramaMasLarga) t
--		Por definicion del (.)
--				V t :: Tree a . heightT ​t = ​ length (ramaMasLarga t)
--		Por principio de induccion estructural en la estructura de t (t :: Tree a)

-- Caso base. t = EmpytT
-- 				heightT ​EmpytT = ​ length (ramaMasLarga EmpytT)

-- length (ramaMasLarga EmpytT)
-- =						(def ramaMasLarga.1)
-- length []
-- =						(def length.1)
-- 0
-- =						(def heightT.1)
-- heightT ​EmpytT

-- Caso Inductivo. t = NodeT x t1 t2
--				HI1:	heightT ​t1 = ​ length (ramaMasLarga t1)
--				HI2:  	heightT ​t2 = ​ length (ramaMasLarga t2)
-- 				TI:		heightT ​(NodeT x t1 t2) = ​ length (ramaMasLarga t2)

-- heightT ​(NodeT x t1 t2)
-- =						(def heightT.2)
-- 1 + max (heightT t1) (heightT t2)
-- =						(HI1)
-- 1 + max (length (ramaMasLarga t1)) (heightT t2)
-- =						(HI1)
-- 1 + max (length (ramaMasLarga t1)) (length (ramaMasLarga t2))

-- length (ramaMasLarga (NodeT x t1 t2))
-- =						(def ramaMasLarga.2)
-- length (x : ramaMasAlta (ramaMasLarga t1) (ramaMasLarga t2)))
-- =						(def length.2)
-- 1 + length (ramaMasAlta (ramaMasLarga t1) (ramaMasLarga t2))
-- =						(lemaGalera)
-- 1 + max (length (ramaMasLarga t1)) (length (ramaMasLarga t2))

-- lemaGalera

-- Prop: length (ramaMasAlta xs ys) =  max (length xs) (length ys)
-- Dem: Por pricipio de induccion estructural en la estructura de xs (xs :: [a]):
-- Caso base: xs = []
--				length (ramaMasAlta [] ys) =  max (length []) (length ys)
-- length (ramaMasAlta [] ys)
-- =					(def ramaMasAlta.1)
-- length ys

-- max (length []) (length ys)
-- =					(def length.1)
-- max 0 (length ys)
-- =					(def max /identidad)
-- length ys

-- Caso Inductivo: xs = x:xs'
--				length (ramaMasAlta (x:xs') ys) =  max (length (x:xs')) (length ys)
--Caso base: ys = []
--				length (ramaMasAlta (x:xs') []) =  max (length (x:xs')) (length [])
-- length (ramaMasAlta (x:xs') []) 			
-- =					(def ramaMasAlta.2)
-- length (x:xs')
-- =					(def length.2)
-- 1 + length xs'

-- max (length (x:xs')) (length [])
-- =					(def length.1)
-- max (length (x:xs')) 0
-- =					(def max/Identidad)
-- length (x:xs')
-- =					(def length.2)
-- 1 + length xs'

-- Caso Inductivo: ys = y:ys'
--				HI: length (ramaMasAlta xs' ys') =  max (length xs') (length ys')
--				TI: length (ramaMasAlta (x:xs') (y:ys')) =  max (length (x:xs')) (length (y:ys'))

-- length (ramaMasAlta (x:xs') (y:ys'))
-- =								(def ramaMasAlta.3)
-- Caso length (x:xs') >  length (y:ys')
-- length (x:xs')
-- =					(def length.2)
-- 1 + length xs' 

-- max (length (x:xs')) (length (y:ys')
-- =					(def max)
-- length (x:xs')
-- =					(def length.2)
-- 1 + length xs' 

-- Caso length (x:xs') <= length (y:ys')
-- length (y:ys')
-- =					(def length.2)
-- 1 + length ys' 

-- max (length (x:xs')) (length (y:ys')
-- =					(def max)
-- length (y:ys')
-- =					(def length.2)
-- 1 + length ys' 

-- Prop: reverse . listInOrder ​ = ​ listInOrder . mirrorT
-- Dem: Por principio de extensionalidad:
-- 			V t :: Tree a . (reverse . listInOrder) ​t = ​ (listInOrder . mirrorT) t
--    	Por definicion (.)
-- 			V t :: Tree a . reverse (listInOrder ​t) = ​ listInOrder (mirrorT t)
-- 		Por induccion estructural en la estructura de t.

-- Caso base: t = EmptyT
-- reverse (listInOrder ​EmptyT) = ​ listInOrder (mirrorT EmptyT)

-- reverse (listInOrder ​EmptyT)
-- =					(def listInOrder.1)
-- reverse []
-- =					(def reverse.1)
-- []

-- listInOrder (mirrorT EmptyT)
-- =					(def mirrorT.1)
-- listInOrder EmptyT
-- =					(def listInOrder.1)
-- []

-- Caso Inductivo: t = NodeT x t1 t2
--		HI1: reverse (listInOrder ​t1) = ​ listInOrder (mirrorT t1)
--		HI2: reverse (listInOrder ​t2) = ​ listInOrder (mirrorT t2)
-- 		TI: reverse (listInOrder ​(NodeT x t1 t2)) = ​ listInOrder (mirrorT (NodeT x t1 t2))

-- reverse (listInOrder ​(NodeT x t1 t2))
-- =					(def listInOrder.2)
-- reverse ((listInOrder t1) ++ [x] ++ (listInOrder t2))
-- =					(Asoc.)
-- reverse ((listInOrder t1) ++ [x]++ (listInOrder t2))
-- =					(lema)
-- reverse (listInOrder ​t2) ++ [x] ++ reverse (listInOrder ​t1)

-- listInOrder (mirrorT (NodeT x t1 t2))
-- =					(def mirrorT.2)
-- listInOrder (NodeT x (mirrorT t2) (mirrorT t1)))
-- =					(def listInOrder.2)
-- listInOrder (mirrorT t2) ++ [x] ++ listInOrder (mirrorT t1)
-- =					(HI2)
-- reverse (listInOrder ​t2) ++ [x] ++ listInOrder (mirrorT t1)
-- =					(HI1)
-- reverse (listInOrder ​t2) ++ [x] ++ reverse (listInOrder ​t1)

-- lema
-- Prop:reverse (xs ++ [x] ++ ys) = reverse ys ++ [x] ++ reverse xs
-- Dem: Po principio de induccion estructural en la estructura de xs

-- Casp base xs = []
-- reverse ([] ++ [x] ++ ys)
-- =					(Asoc)
-- reverse (([] ++ [x]) ++ ys)
-- =					(def (++).1)
-- reverse ([x] ++ ys)
-- =					(def (++).2)
-- reverse (x: ([] ++ ys))
-- =					(def (++).1)
-- reverse (x: ys)
-- =					(def reverse.2)
-- reverse ys ++ [x]

-- reverse ys ++ [x] ++ reverse []
-- =					(def reverse.1)
-- reverse ys ++ [x] ++ []
-- =					(lemita)
-- reverse ys ++ [x]

-- Casp Inductivo xs = x' : xs'
-- 			HI: reverse (xs' ++ [x] ++ ys) = reverse ys ++ [x] ++ reverse xs'
-- 			TI: reverse ((x' : xs') ++ [x] ++ ys) = reverse ys ++ [x] ++ reverse (x' : xs')

-- reverse ((x' : xs') ++ [x] ++ ys)
-- =					(Asoc)
-- reverse (((x' : xs') ++ [x]) ++ ys)
-- =					(def (++).2)
-- reverse (x' : (xs' ++ [x]) ++ ys)
-- =					(def reverse.2)
-- reverse ((xs' ++ [x]) ++ ys) ++ [x']
-- =					(Asoc)
-- reverse (xs' ++ [x] ++ ys) ++ [x']
-- =					(Asoc, HI)
-- reverse ys ++ [x] ++ reverse xs' ++ [x']
-- =					(Asoc)
-- reverse ys ++ [x] ++ (reverse xs' ++ [x'])
-- =					(ef reverse.2)
-- reverse ys ++ [x] ++ reverse (x':xs')

-- lemita
-- Prop: xs ++ [] = xs
-- Dem: Por principio de induccion estructural en la estructura de ys
-- Caso base xs = []
-- 		[] ++ [] = []
-- [] ++ []
-- =		(def (++).1)
-- []

-- Caso inductivo xs = x:xs'
-- 		HI: xs' ++ [] = xs'
-- 		TI: (x:xs') ++ [] = x:xs'

-- (x : xs') ++ []
-- =		(def (++).2)
-- x : (xs' ++ [])
-- =		(HI)
-- x : xs'

-- Ejercicio 4)
-- Dada la siguiente definición

data AppList a = Single a | Append (AppList a) (AppList a) deriving Show

ejemploAP :: AppList Int
ejemploAP = Append (Single 2) 
				   (Append (Append (Single 4)
				   	               (Single 8)) 
				   	       (Single 16))
foldAppList :: (a -> b) -> (b -> b -> b) -> AppList a -> b
foldAppList s a (Single x) = s x
foldAppList s a (Append l1 l2) = a (foldAppList s a l1) (foldAppList s a l2)

recrAppList :: (a -> b) -> (AppList a -> b -> AppList a -> b -> b) -> AppList a -> b
recrAppList s a (Single x) = s x
recrAppList s a (Append l1 l2) = a l1 (recrAppList s a l1) l2 (recrAppList s a l2)

-- cuya intención es describir una representación no lineal de listas no vacías;

-- implementar las siguientes funciones:

lenAL :: AppList a -> Int
-- que describe la cantidad de elementos de la lista.
lenAL (Single _) = 1
lenAL (Append l1 l2) = lenAL l1 + lenAL l2

-- consAL :: a -> AppList a -> AppList a
-- que describe la lista resultante de agregar el elemento dado al principio de la lista dada.

-- headAL :: AppList a -> a
-- que describe el primer elemento de la lista dada.

-- tailAL :: AppList a -> AppList a
-- que describe la lista resultante de quitar el primer elemento de la lista dada.

-- snocAL :: a -> AppList a -> AppList a​
-- que describe la lista resultante de agregar el elemento dado al final de la lista dada.

-- lastAL :: AppList a -> a
-- que describe el último elemento de la lista dada.

-- initAL :: AppList a -> AppList a​
-- que describe la lista dada sin su último elemento.

-- reverseAL :: AppList a -> AppList a
-- que describe la lista dada con sus elementos en orden inverso.

-- elemAL :: Eq a => a -> AppList a -> Bool
-- que indica si el elemento dado se encuentra en la lista dada.

-- appendAL :: AppList a -> AppList a -> AppList a
-- que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
-- NOTA: buscar la manera más eficiente de hacerlo.

-- appListToList :: AppList a -> [a]​
-- que describe la representación lineal de la lista dada.


-- Ejercicio 5)
-- Dadas las siguientes definiciones

data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
data Color = RGB Int Int Int
type Image = QuadTree Color

foldQT :: (a -> b) -> (b -> b -> b -> b -> b) -> QuadTree a -> b
foldQT l n (LeafQ x) =  l x
foldQT l n (NodeQ q1 q2 q3 q4) = n (foldQT l n q1) (foldQT l n q2) (foldQT l n q3) (foldQT l n q4) 

recrQT :: (a -> b) -> (QuadTree a -> b ->QuadTree a -> b -> QuadTree a -> b -> QuadTree a -> b -> b) -> QuadTree a -> b
recrQT l n (LeafQ x) =  l x
recrQT l n (NodeQ q1 q2 q3 q4) = n q1 (recrQT l n q1) q2 (recrQT l n q2) q3 (recrQT l n q3) q4 (recrQT l n q4) 

-- donde ​ QuadTree representa a la estructura de los árboles cuaternarios, ​ Color
-- representa a los colores con precisión ​ TrueColor ​ , e ​ Image representa imágenes
-- cuadradas de tamaños arbitrarios;


-- implementar las siguientes funciones:

heightQT :: QuadTree a -> Int​
-- que describe la altura del árbol dado.
heightQT (LeafQ _) = 1
heightQT (NodeQ q1 q2 q3 q4) = 1 + max (max (heightQT q1) (heightQT q2)) (max (heightQT q3) (heightQT q4))

countLeavesQT :: QuadTree a -> QuadTree a
-- que describe la cantidad de hojas del árbol dado.
countLeavesQT (LeafQ _) = 1
countLeavesQT (NodeQ q1 q2 q3 q4) = countLeavesQT q1 + countLeavesQT q2 + countLeavesQT q3 + countLeavesQT q4

sizeQT :: QuadTree a -> Int
-- que describe la cantidad de constructores del árbol dado.
sizeQT (LeafQ _) = 1
sizeQT (NodeQ q1 q2 q3 q4) = 1 + sizeQT q1 +  sizeQT q2 +  sizeQT q3 +  sizeQT q4

-- areEquals :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> Bool
-- areEquals () () () () =  
compress :: QuadTree a -> QuadTree a
-- que describe el árbol resultante de transformar en hoja todos aquellos nodos para los
-- que se cumpla que todos los elementos de sus subárboles son iguales.
compress (LeafQ x) = LeafQ x
compress (NodeQ q1 q2 q3 q4) = if areEquals q1 q2 q3 q4
										then NodeQ (value q1) (value q2) (value q3) (value q4)
										else NodeQ (compress q1) (compress q2) (compress q3) (compress q4)

-- uncompress :: QuadTree a -> QuadTree a​
-- que describe el árbol resultante de transformar en nodo, manteniendo el dato de la
-- hoja correspondiente, todas aquellas hojas que no se encuentren en el nivel de la altura del árbol.

-- render :: Image -> Int -> Image​
-- que describe la imagen dada en el tamaño dado.
-- Precondición: el tamaño es potencia de 4 y su raiz cuarta es mayor o igual a la altura del árbol dado.
-- NOTA: Una imagen tiene tamaño ​ t cuando todas las hojas se encuentran en el nivel ∜​ t ​ .