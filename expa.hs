data ExpA = Cte Int
			| Sum ExpA ExpA
			| Prod ExpA ExpA deriving Show

ejemplo :: ExpA
ejemplo = Sum (Prod (Cte 2)
					(Cte 1))  
			  (Prod (Cte 1)
			  		(Sum (Cte 0)
			  			 (Cte 4)))

evalEA :: ExpA -> Int​
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA (Cte n) = n
evalEA (Sum e1 e2) = (+)  (evalEA e1) (evalEA e2) 
evalEA (Prod e1 e2) = (*) (evalEA e1) (evalEA e2) 

simplificarEA :: ExpA -> ExpA​ 
--que describe una expresión aritmética con el mismo significado que la dada, pero que no tiene
--sumas del número 0, ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente ​ simbólica . ​
simplificarEA (Cte n) = Cte n
simplificarEA (Sum e1 e2) = armarSuma (simplificarEA e1) (simplificarEA e2) 
simplificarEA (Prod e1 e2) = armarProd (simplificarEA e1) (simplificarEA e2)

armarSuma :: ExpA -> ExpA -> ExpA
armarSuma (Cte 0) e2 = e2
armarSuma e1 (Cte 0) = e1
armarSuma e1 e2 = Sum e1 e2

armarProd :: ExpA -> ExpA -> ExpA
armarProd (Cte 1) e2 = e2
armarProd e1 (Cte 1) = e1
armarProd e1 e2 = Prod e1 e2


contarCeros :: ExpA -> Int
contarCeros (Cte 0) = 1
contarCeros _ = 0

cantidadDeSumaCero :: ExpA -> Int​
--que describe la cantidad de veces que aparece suma cero en la expresión aritmética dada. La
--resolución debe ser exclusivamente ​ simbólica . ​
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Sum e1 e2) = contarCeros e1 + contarCeros e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

foldExpA :: (Int -> b)-> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n) = c n
foldExpA c s p (Sum e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)  
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)    

recExpA :: (Int -> b)-> (ExpA -> b ->ExpA -> b -> b) -> (ExpA -> b ->ExpA -> b -> b) -> ExpA -> b
recExpA c s p (Cte n) = c n
recExpA c s p (Sum e1 e2) = s e1 (recExpA c s p e1) e2 (recExpA c s p e2)  
recExpA c s p (Prod e1 e2) = p e1 (recExpA c s p e1) e2 (recExpA c s p e2)    

evalEA' :: ExpA -> Int​
evalEA' = foldExpA id (+) (*)

simplificarEA' :: ExpA -> ExpA
simplificarEA' = foldExpA Cte armarSuma armarProd

cantidadDeSumaCero' :: ExpA -> Int​
cantidadDeSumaCero' = recExpA (const 0) (\e1 c1 e2 c2 -> contarCeros e1 + contarCeros e2 + c1 + c2) (\_ c1 _ c2 -> c1 + c2)



--Ejercicio 1)
--Dada la siguiente representación de expresiones aritméticas

data EA = Const Int | BOp BinOp EA EA
data BinOp = Suma | Mul

ejemplo1 :: EA
ejemplo1 = BOp Suma (BOp Mul (Const 2)
					(Const 1))  
			  (BOp Mul (Const 1)
			  		(BOp Suma (Const 0)
			  			 (Const 4)))

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA c b (Const n) = c n 
foldEA c b (BOp binop e1 e2) = b binop (foldEA c b e1) (foldEA c b e2)

recEA :: (Int -> b) -> (BinOp -> EA -> b -> EA -> b -> b) -> EA -> b
recEA c b (Const n) = c n    
recEA c b (BOp binop e1 e2) = b binop e1 (recEA c b e1) e2 (recEA c b e2)  

evalEA2 :: EA -> Int
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA2 (Const n) = n
evalEA2 (BOp binop e1 e2) = evalBinop binop (evalEA2 e1) (evalEA2 e2)

evalBinop :: BinOp -> Int -> Int -> Int 
evalBinop Suma = (+)
evalBinop Mul = (*)

ea2ExpA :: EA -> ExpA
--que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la dada.
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp binop e1 e2) = binop2ea binop (ea2ExpA e1) (ea2ExpA e2)

binop2ea :: BinOp -> ExpA -> ExpA -> ExpA 
binop2ea Suma = Sum
binop2ea Mul = Prod 


evalEA2' :: EA -> Int
evalEA2' = foldEA id evalBinop

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte binop2ea