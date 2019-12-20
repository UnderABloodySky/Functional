-- Ejercicio 1) Dar las reglas que definen el conjunto inductivo correspondiente a la
-- parte totalmente definida del tipo algebraico ​ Pizza​ definido por:

data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa

--Ingrediente:
--		Aceitunas E Ingrediente
--		Anchoas E Ingrediente
--		Cebolla E  Ingrediente
--		Jamon Ingrediente
--		Queso E  Ingrediente
--		Salsa E Ingrediente

-- Pizza:
--		Prepizza  E Pizza 
--		Si p  E Pizza  && i  E Ingrediente 
--			Entonces Capa i p E Pizza
--		&& es el menor.


-- Ejercicio 2) Dar la formación esquemática de una función definida por recursión
-- estructural sobre el tipo trabajado en el ejercicio anterior.

-- f :: Pizza -> a
-- f Prepizza = ...
-- f (Capa i p) = ... i ... (f p)


-- Ejercicio 3) Dar el tipo de cada una de las siguientes funciones, y definirlas utilizando
-- recursión estructural.

cantidadDeCapas :: Pizza -> Int
-- que describe la cantidad de capas de ingredientes de la misma.
cantidadDeCapas Prepizza = 0 
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas​ :: Pizza -> Int
-- que describe la cantidad de aceitunas que hay en una pizza dada.
cantidadDeAceitunas​ Prepizza = 0
cantidadDeAceitunas​ (Capa i p) = cantidad i + cantidadDeAceitunas p

cantidad :: Ingrediente -> Int
cantidad (Aceitunas n) = n 
cantidad _ = 0

duplicarAceitunas​ :: Pizza -> Pizza
-- que dada una pizza, describe otra pizza de forma tal que se cumpla la siguiente propiedad:
-- cantidadDeAceitunas (duplicarAceitunas p) = ​ 2 * cantidadDeAceitunas p
duplicarAceitunas Prepizza = Prepizza 
duplicarAceitunas (Capa i p) = Capa (duplicar i) (duplicarAceitunas p)

duplicar :: Ingrediente -> ingredientes
duplicar (Aceitunas n) = Aceitunas (n * 2)
duplicar i = i 

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

sinLactosa :: Pizza -> Pizza
-- indica la pizza resultante de remover todas las capas de queso de una pizza dada.
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = if esQueso i
								then sinLactosa p
								else Capa i (sinLactosa p)

aptaIntolerantesLactosa :: Pizza -> Bool
-- que indica si la pizza dada no tiene queso, o sea se cumple la siguiente propiedad:
-- si ​ aptaIntolerantesLactosa p ​ = ​ True
-- 		entonces ​ p ​ = ​ sinLactosa p
aptaIntolerantesLactosa Prepizza = True 
aptaIntolerantesLactosa (Capa i p) = (not . esQueso) i && aptaIntolerantesLactosa p

esCorrecto :: Ingrediente -> Bool
esCorrecto (Aceitunas n) = n > 0 
esCorrecto _ = True

agregaAceitunasCorrectamente​ :: Pizza -> Bool
-- que indica si en la pizza dada nunca se agrega una cantidad negativa o cero de aceitunas.
agregaAceitunasCorrectamente​ Prepizza = True 
agregaAceitunasCorrectamente​ (Capa i p) = esCorrecto i && agregaAceitunasCorrectamente​ p

juntar :: Ingrediente -> Pizza -> Pizza
juntar (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntar i p = Capa i p

conDescripcionMejorada​ :: Pizza -> Pizza
-- que toma una pizza que agrega aceitunas correctamente y describe otra pizza que se construyó con exactamente los
-- mismos ingredientes pero donde no se agregan aceitunas dos veces seguidas.
conDescripcionMejorada​  Prepizza = Prepizza
conDescripcionMejorada​  (Capa i p) = juntar i conDescripcionMejorada​ p

esMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
esMismoIngrediente (Aceitunas n)  (Aceitunas m) = n == m 
esMismoIngrediente Anchoas Anchoas = True
esMismoIngrediente Cebolla Cebolla = True
esMismoIngrediente Jamon Jamon = True
esMismoIngrediente Queso Queso = True
esMismoIngrediente Salsa Salsa = True
esMismoIngrediente _ _ = False

tiene​ :: Ingrediente ->  Pizza -> Pizza
-- que toma un ingrediente y una pizza e indica si la pizza dada tiene el ingrediente dado.
tiene i Prepizza = False
tiene i (Capa i' p) = esMismoIngrediente i i' || tiene i p 

-- Prop: cantidadDeAceitunas (duplicarAceitunas p) = ​ 2 * cantidadDeAceitunas p
-- Dem: Por principio de induccion estructural en la estructura de p (p :: Pizza)

-- Caso base: p = Prepizza

-- cantidadDeAceitunas (duplicarAceitunas Prepizza)
-- =									(def duplicarAceitunas.1)
-- cantidadDeAceitunas Prepizza
-- =									(def cantidadDeAceitunas.1)
-- 0

-- 2 * cantidadDeAceitunas Prepizza
-- =									(def cantidadDeAceitunas.1)
-- 2 * 0
-- =									(aritmetica)
-- 0