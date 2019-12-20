-- Ejercicio 1)
-- Dadas las siguientes definiciones

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving Show

data Helado =	Vasito Gusto
			  	| Cucurucho Gusto Gusto
				| Pote Gusto Gusto Gusto deriving Show

agregarChocolate consH = consH Chocolate

-- determinar el tipo de las siguientes expresiones:

-- Vasito :: Gusto -> Helado
-- Chocolate :: Gusto
-- Cucurucho :: Gusto -> Gusto -> Helado
-- Sambayón :: Gusto
-- Pote :: Gusto -> Gusto -> Gusto -> Helado
-- agregarChocolate :: (Gusto -> a) -> a 
-- agregarChocolate Vasito :: Helado
-- agregarChocolate Cucurucho :: Gusto -> Helado
-- agregarChocolate (Cucurucho Sambayon) :: Helado
-- agregarChocolate (agregarChocolate Cucurucho) :: Helado
-- agregarChocolate (Vasito DulceDeLeche) -- X
-- agregarChocolate Pote :: Gusto -> Gusto -> Helado
-- agregarChocolate (agregarChocolate (Pote Frutilla)) :: Helado


-- Ejercicio 2)
-- Dado el siguiente tipo que pretende representar dígitos binarios

data DigBin = O | I

-- definir las siguientes funciones:

dbAsInt :: DigBin -> Int
-- que dado un símbolo que representa un dígito binario lo transforma en su significado como número.
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Bool​
-- que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano.
dbAsBool O = False
dbAsBool I = True

dbOfBool :: Bool -> DigBin
--que dado un booleano lo transforma en el símbolo que representa a ese booleano.
dbOfBool False = O
dbOfBool True = I

negDB :: DigBin -> DigBin​
-- que dado un dígito binario lo transforma en el otro.
negDB O = I
negDB I = O


-- Ejercicio 3)
-- Dado el siguiente tipo que pretende representar dígitos binarios

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

-- definir las siguientes funciones:

ddAsInt :: DigDec -> Int
--que dado un símbolo que representa un dígito decimal lo transforma en su significado como número.
ddAsInt D0 = 0 
ddAsInt D1 = 1
ddAsInt D2 = 2 
ddAsInt D3 = 3 
ddAsInt D4 = 4 
ddAsInt D5 = 5 
ddAsInt D6 = 6 
ddAsInt D7 = 7 
ddAsInt D8 = 8 
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec​
-- que dado un número entre 0 y 9 lo transforma en el símbolo que representa a ese dígito.
ddOfInt 0 = D0 
ddOfInt 1 = D1 
ddOfInt 2 = D2 
ddOfInt 3 = D3 
ddOfInt 4 = D4 
ddOfInt 5 = D5 
ddOfInt 6 = D6 
ddOfInt 7 = D7 
ddOfInt 8 = D8 
ddOfInt 9 = D9 

nextDD :: DigDec -> DigDec​
-- que dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición.
nextDD D0 = D1 
nextDD D1 = D2
nextDD D2 = D3 
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6 
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

prevDD :: DigDec -> DigDec​
-- que dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición.
prevDD D0 = D9 
prevDD D1 = D0
prevDD D2 = D1 
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4 
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8


-- Ejercicio 4) Dado el siguiente tipo que representa medidas en un software de dibujo
--  como LibreOffice Draw.

data Medida = Mm Float | Cm Float | Inch Float | Foot Float deriving Show

-- y la siguiente tabla de conversión

asMm :: Medida -> Medida​
-- que dada una medida cualquiera transforma en una medida en milímetros que aproxima la dada según conversión establecida.
asMm (Mm f) = Mm f
asMm (Cm f) = Mm (f * 10)  
asMm (Inch f) = Mm (f * 25.04)  
asMm (Foot f) = Mm (f * 304.8) 

asCm :: Medida -> Medida​
-- que dada una medida cualquiera transforma en una medida en centímetros que aproxima la dada según conversión establecida.
asCm (Mm f) = Cm (f / 10) 
asCm (Cm f) = Cm f 
asCm (Inch f) = Cm (f * 2.54) 
asCm (Foot f) = Cm (f * 30.48)

asInch :: Medida -> Medida​
-- que dada una medida cualquiera transforma en una medida en pulgadas que aproxima la dada según conversión establecida.
asInch (Mm f) = Inch (f / 0.039) 
asInch (Cm f) = Inch (f / 0.394)  
asInch (Inch f) = Inch f 
asInch (Foot f) = Inch (f * 12) 

asFoot :: Medida -> Medida​
-- que dada una medida cualquiera la transforma en una medida en pies que aproxima la dada según la conversión establecida.
asFoot (Mm f) = Foot (f / 0.003)
asFoot (Cm f) = Foot (f / 0.033)
asFoot (Inch f) = Foot (f / 0.083)
asFoot (Foot f) = Foot f


-- Ejercicio 5)
-- Determinar el tipo de las siguientes expresiones:
-- Ejercicio 6) 
-- Para cada una de las expresiones del ejercicio anterior que denoten funciones, construir una expresión aplicándola.

-- uncurry Rect
data Shape = Rect Float Float | Circle Float 

-- construyeShNormal (flip Rect 5.0)
-- 

-- compose (uncurry Rect) swap :: (Float, Float) -> Shape
--compose (uncurry Rect) swap (2.0, 4.8) => Rect 4.9 2.0

-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado
-- uncurry Cucurucho (DulceDeLeche, Frutilla) => Cucurucho DulceDeLeche Frutilla 

swap (x, y) = (y, x)
-- uncurry Rect swap 	X

compose f g = f . g 
-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
-- compose uncurry Pote Chocolate (DulceDeLeche, Frutilla) -> Pote Chocolate DulceDeLeche Frutilla

-- compose Just :: (a -> b) -> a -> Maybe b
-- compose Just doble 4 => Just 8

-- compose uncurry (Pote Chocolate) 	X


-- Ejercicio 7)
-- Dados los siguientes tipos que modelan los posibles resultados de computar con excepciones

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

-- definir la función

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
-- que dada una computación que puede fallar, una función que indica cómo continuar si no falla, y un manejador de los casos de falla, expresa la computación completa.
tryCatch (Raise except) _ handler = handler except
tryCatch (Ok value) f _ = f value 

-- Un ejemplo que utiliza esta función sería el siguiente:
-- sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
-- sueldoGUIE nombre empleados = tryCatch (lookupE nombre empleados)
-- 											mostrarInt
--											(\e -> case e of
--												NotFound -> ventanaError msgNotEmployee
--												_ -> error msgUnexpected)
--											where msgNotEmployee = "No es empleado de la empresa"
--												  msgUnexpected = "Error inesperado"

-- sabiendo que
-- mostrarInt :: Int -> GUI Int
-- ventanaError :: String -> GUI a
-- lookupE :: Nombre -> [Empleado] -> MayFail Int