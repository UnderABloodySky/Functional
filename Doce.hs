data ExpA = Cte Int | Sum ExpA ExpA | Prod ExpA ExpA deriving Show
-- Dar el tipo y definir ​ foldExpA​ , que expresa el esquema de recursión estructural para la estructura ​ ExpA​ .
-- Resolver las siguientes funciones utilizando ​ foldExpA
​
foldExpA​ :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA​ c _ _ (Cte n) = c n
foldExpA​ c s p (Sum e1 e2) = s (foldExpA​ c s p e1) (foldExpA​ c s p e2) 
foldExpA​ c s p (Prod e1 e2)= p (foldExpA​ c s p e1) (foldExpA​ c s p e2) 

unoSiEsCero :: Int -> Int
unoSiEsCero 0 = 1
unoSiEsCero _ = 0

cantidadDeCeros :: ExpA -> Int​
-- que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros = foldExpA​ unoSiEsCero (+) (+)

esNegativo :: Int -> Bool
esNegativo n = n < 0

noTieneNegativosExplicitosExpA :: ExpA -> Bool
-- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosExpA = foldExpA​ (not . esNegativo) (&&) (&&)

simplificarExpA' :: ExpA -> ExpA
-- que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución
-- debe ser exclusivamente ​ simbólica . ​
simplificarExpA' = foldExpA​ Cte simpSuma simpProd

simpSuma :: ExpA -> ExpA -> ExpA
simpSuma (Cte 0) e2 = e2
simpSuma e1 (Cte 0) = e1
simpSuma (Cte n) (Cte m) = Cte (n+m)
simpSuma e1 e2 = Sum e1 e2

simpProd :: ExpA -> ExpA -> ExpA
simpProd (Cte 0) e2 = Cte 0
simpProd e1 (Cte 0) = Cte 0
simpProd (Cte 1) e2 = e2
simpProd e1 (Cte 1) = e1
simpProd (Cte n) (Cte m) = Cte (n*m)
simpProd e1 e2 = Prod e1 e2

evalExpA' :: ExpA -> Int​ 
-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' = foldExpA​ id (+) (*)

showExpA :: ExpA -> String​
-- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showExpA = foldExpA​ show (\str1 str2 -> "(" ++ str1 ++ "+" ++ str2) (\str1 str2 -> "(" ++ str1 ++ "*" ++ str2)


-- Ejercicio 2)
-- Dada la definición de ​ EA​ :

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Suma | Mul deriving Show

-- Dar el tipo y definir ​ foldEA​ , que expresa el esquema de recursión estructural para la estructura ​ EA​ .

foldEA​ :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA​ c b (Const n) = c n
foldEA​ c b (BOp binop e1 e2) =b binop (foldEA​ c b e1) (foldEA​ c b e2)

-- Resolver las siguientes funciones utilizando ​ foldEA​ :
noTieneNegativosExplicitosEA :: EA -> Bool​
-- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosEA = foldEA​ (not . esNegativo) (const (&&))

armarBOp :: BinOp -> EA -> EA -> EA
armarBOp Suma (Const 0) e = e
armarBOp Suma e (Const 0) = e
armarBOp Mul (Const 0) e = Const 0
armarBOp Mul e (Const 0) = Const 0
armarBOp Mul (Const 1) e = e
armarBOp Mul e (Const 1) = e
armarBOp Suma (Const n) (Const m) = Const (n + m)
armarBOp Mul (Const n) (Const m) = Const (n * m)
armarBOp bop e1 e2 = BOp bop e1 e2

simplificarEA' :: EA -> EA​
-- que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser
-- exclusivamente ​ simbólica ​ .
simplificarEA' = foldEA​ Const armarBOp 

evalEA' :: EA -> Int​
-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA' = foldEA​ id evalBinop

evalBinop :: BinOp -> Int -> Int -> Int
evalBinop Suma = (+)
evalBinop Mul = (*)

showEA :: EA -> String​
-- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showEA = foldEA​ show showBinop

showBinop :: BinOp -> String -> String -> String
showBinop Suma = \str1 str2 -> "(" ++ str1 ++ "+" ++ str2 ++ ")" 
showBinop Mul = \str1 str2 -> "(" ++ str1 ++ "*" ++ str2 ++ ")"

ea2ExpA' :: EA -> ExpA​
-- que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la dada.
ea2ExpA' = foldEA​ Cte binOp2ExpA 

binOp2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
binOp2ExpA Suma = Sum 
binOp2ExpA Mul = Prod

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b) deriving Show

ea2Arbol' :: EA -> Arbol BinOp Int​
-- que describe la representación como elemento del tipo ​ Arbol BinOp Int de la expresión aritmética dada
ea2Arbol' = foldEA​ Hoja Nodo
