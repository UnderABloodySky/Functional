--TP3 ED
-- Arboles binarios
-- Mapas
-- Demostrar:

--			hayTesoro m = or (map (elem Tesoro) (tesorosPerLevel m)
--			where foldr (||) False


--1. Árboles binarios
--Defina las siguientes funciones sobre árboles binarios utilizando recursión estructural según
--corresponda:

data Tree a = 	Leaf a 
				| NodeT a  (Tree a) (Tree a) deriving Show

ejemplo = NodeT 2 (NodeT 4 (NodeT 8 (Leaf 1024) 
	                                (Leaf 256))
	                       (NodeT 16 (Leaf 64)
	                       			(Leaf 128)))
 				  (NodeT 42 (Leaf 32)
 				  	        (Leaf 0))
sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT (Leaf n) = n
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT (Leaf _) = 1
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT (Leaf n) = Leaf (porDos n)
mapDobleT (NodeT x t1 t2) = NodeT (porDos x) (mapDobleT t1) (mapDobleT t2)

porDos :: Int -> Int
porDos n = 2 * n

mapLongitudT :: Tree String -> Tree Int
--Dado un árbol de palabras devuelve un árbol con la longitud de cada palabra.
mapLongitudT (Leaf p) = Leaf (length p)
mapLongitudT (NodeT p t1 t2) = NodeT (length p) (mapLongitudT t1) (mapLongitudT t2) 

perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT x (Leaf x') = x == x'
perteneceT x (NodeT x' t1 t2) = x == x' || perteneceT x t1 || perteneceT x t2 


aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT _ (Leaf n) = 0
aparicionesT x (NodeT x' t1 t2) = unoSi (x == x') + aparicionesT x t1 + aparicionesT x t2

countLeaves :: Tree a -> Int
--Dado un árbol devuelve su cantidad de hojas.
--Nota: una hoja (leaf en inglés) es un nodo que no tiene hijos.
countLeaves (Leaf n) = 1
countLeaves (NodeT x' t1 t2) = countLeaves t1 + countLeaves t2

leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves (Leaf x) = [x]
leaves (NodeT _ t1 t2) = leaves t1 ++ leaves t2 

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol 1 . La altura de un árbol vacío es cero y la de una hoja es 1.
heightT (Leaf _) = 1 
heightT (NodeT _ t1 t2) = (+) 1  (max (heightT t1) (heightT t2))

countNotLeaves :: Tree a -> Int
--Dado un árbol devuelve el número de nodos que no son hojas. ¿Cómo podría resolverla sin
--utilizar recursión explícita? Primero defínala con recursión explícita y después sin ella.
countNotLeaves (Leaf _) = 0
countNotLeaves (NodeT _ t1 t2) = 1 + countNotLeaves t1 + countNotLeaves t2

mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT (Leaf x) = Leaf x 
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1) 

listInOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
listInOrder (Leaf x) = [x]
listInOrder (NodeT x t1 t2) = listInOrder t1 ++ [x] ++ listInOrder t2

listPreOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
--Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
--a continuación los elementos del hijo derecho.
listPreOrder (Leaf x) = [x]
listPreOrder (NodeT x t1 t2) = [x] ++ listPreOrder t1 ++ listPreOrder t2 

listPosOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo post-
--order.
--Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, a conti-
listPosOrder (Leaf x) = [x]
listPosOrder (NodeT x t1 t2) = listPosOrder t1 ++ listPosOrder t2 ++ [x] 

concatenarListasT :: Tree [a] -> [a]
--Dado un árbol de listas devuelve la concatenación de todas esas listas. El recorrido debe ser
--in-order.
concatenarListasT (Leaf xs) = xs
concatenarListasT (NodeT xs t1 t2) = concatenarListasT t1 ++ xs ++ concatenarListasT t2  

value :: Tree a -> [a]
value (Leaf x) = [x]
value (NodeT x t1 t2) = [x]

levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN 0 t = value t
levelN n (NodeT xs t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel (Leaf x) = [[x]]
listPerLevel (NodeT x t1 t2) =   [x] : merge (listPerLevel t1) (listPerLevel t2)

merge :: [[a]] -> [[a]] -> [[a]]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga (Leaf x) = [x]
ramaMasLarga (NodeT x t1 t2) = x : ramaMasLarga (ramaMasAlta t1 t2)

ramaMasAlta :: Tree a -> Tree a -> Tree a
ramaMasAlta t1 t2 = if heightT t1 > heightT t2
							then t1
							else t2

agregar :: a -> [[a]] -> [[a]]
agregar _ [] = []
agregar x (ys:yss) = (x:ys) : agregar x yss 

todosLosCaminos :: Tree a -> [[a]]
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas. todosLosCaminos (Leaf x) = [[x]]
todosLosCaminos (NodeT x t1 t2) = agregar x (todosLosCaminos t1)  ++ agregar x (todosLosCaminos t2)

balanceado :: Tree a -> Bool
--Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la
--diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado (Leaf _) = True
balanceado (NodeT _ t1 t2) = abs (heightT t1 - heightT t2) <= 1 && balanceado t1 && balanceado t2

foldT :: (a -> b) -> ( a -> b -> b -> b) -> Tree a -> b 
foldT f	g (Leaf x) = f x
foldT f g (NodeT x t1 t2) = g x (foldT f g t1) (foldT f g t2)

suma3 :: Int -> Int -> Int -> Int
suma3 x y z = x + y + z

sumarT' :: Tree Int -> Int
sumarT' = foldT id suma3

sizeT' :: Tree a -> Int
sizeT' = foldT (const 1) (\_ n1 n2 -> succ n1 + n2)

mapDobleT' :: Tree Int -> Tree Int
mapDobleT' = foldT (Leaf . porDos) (\n t1 t2 -> NodeT (porDos n) t1 t2)

mapLongitudT' :: Tree String -> Tree Int
mapLongitudT' = foldT (Leaf . length) (\p t1 t2 -> NodeT (length p) t1 t2)

esIgual :: Eq a => a -> a -> Bool
esIgual x y = x == y

perteneceT' :: Eq a => a -> Tree a -> Bool
perteneceT' = \x -> foldT (esIgual x) (\y b1 b2 -> esIgual x y || b1 || b2)

aparicionesT' :: Eq a => a -> Tree a -> Int
aparicionesT' = \x -> foldT (unoSi . esIgual x) (\y n1 n2 -> unoSi (esIgual x y) + n1 + n2)

countLeaves' :: Tree a -> Int
countLeaves' = foldT (const 1) (\_ n1 n2 -> n1 + n2)

singular :: a -> [a]
singular x = [x]

leaves' :: Tree a -> [a]
leaves' = foldT singular (\x xs ys -> xs ++ ys)

heightT' :: Tree a -> Int
heightT' = foldT (const 1) (\_ n1 n2 -> succ (max n1 n2))

countNotLeaves' :: Tree a -> Int
countNotLeaves' = foldT (const 0) (\_ n1 n2 -> succ (n1 + n2))

mirrorT' :: Tree a -> Tree a
mirrorT' = foldT Leaf (\x t1 t2 -> NodeT x t2 t1)

listInOrder' :: Tree a -> [a]
listInOrder' = foldT singular (\x l1 l2 -> l1 ++ singular x ++ l2)

listPreOrder' :: Tree a -> [a]
listPreOrder' = foldT singular (\x l1 l2 -> singular x ++ l1 ++ l2)

listPosOrder' :: Tree a -> [a]
listPosOrder' = foldT singular (\x l1 l2 -> l1 ++ l2 ++ singular x)

concatenarListasT' :: Tree [a] -> [a]
--Dado un árbol de listas devuelve la concatenación de todas esas listas. El recorrido debe ser
--in-order.
concatenarListasT' = foldT id (\e l1 l2 -> l1 ++ e ++ l2)

levelN' :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN' = flip (foldT (const.singular) (\x fr1 fr2 m -> if m == 0
												then [x]
												else fr1 (m-1) ++ fr2 (m-1)))
listPerLevel' :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel' = foldT (singular . singular) (\x l1 l2 -> [x] : merge l1 l2)

ramaMasLarga' :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga' = foldT (const []) (\x l1 l2 -> if length l1 > length l2
												then x : l1
												else x : l2) 

agregar' :: a -> [[a]] -> [[a]]
agregar' x = map (x:)

todosLosCaminos' :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos' = foldT (\x -> [[x]]) (\x l1 l2 -> agregar' x l1 ++ agregar' x l2)

--balanceado' :: Tree a -> Bool
--Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la
--diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Necesito Rec?

unoSi:: Bool -> Int
unoSi True = 1
unoSi _ = 0

unoSi' :: (a -> Bool) -> a -> Int
unoSi' = \f x -> if f x
					then 1
					else 0


countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT (unoSi.f) (\x n1 n2 -> if f x
										then succ n1 + n2
										else n1 + n2)​

countByT' :: (a -> Bool) -> Tree a -> Int
countByT' f = foldT (unoSi' f) (\x n1 n2 -> if f x
										then succ n1 + n2
										else n1 + n2)​

ejemplo1 :: Tree Int
ejemplo1 = NodeT 2 (NodeT 4 (NodeT 6 (Leaf 8) (Leaf 10)) (NodeT 38 (Leaf 12) (Leaf 14))) (NodeT 16 (NodeT 18 (Leaf 20) (Leaf 22)) (NodeT 24 (NodeT 26 (Leaf 28) (Leaf 30)) (NodeT 32 (Leaf 34) (Leaf 36))))

ejemplo2 :: Tree String
ejemplo2 = NodeT "Hola" (NodeT "Como" (NodeT "Estas" (Leaf "L") (Leaf "H") ) (Leaf "Alo")) (NodeT "Asd" (Leaf "Ahi") (Leaf "Ah"))

ejemplo3 :: Tree Int
ejemplo3 = NodeT 1 (Leaf 4) (NodeT 2 (NodeT 4 (NodeT 2 (NodeT 4(NodeT 2 (NodeT 1 (Leaf 2) (Leaf 2)) (Leaf 2)) (Leaf 2)) (Leaf 3) ) (Leaf 3)) (Leaf 5))

ejemplo4 :: Tree Int
ejemplo4 = NodeT 1 (Leaf 2) (NodeT 3 (Leaf 4) (NodeT 5 (Leaf 6) (Leaf 7)))

-- 	1
--	/\
--	2 3	
--    /\
--   4  5
--   	/\	
--     6  7

ejemplo5 :: Tree [Int]
ejemplo5 = NodeT [1,2,3] (Leaf [2]) (NodeT [3, 4] (Leaf [14, 8]) (NodeT [5] (Leaf [16,34,9]) (Leaf [42, 0, 7])))

--	[1,2,3]
--	 / \
-- [2] [3,4]	
--       /   \
-- [14, 8]  [5]
--     		/ \	
-- [16, 34, 9] [42, 0, 7]

