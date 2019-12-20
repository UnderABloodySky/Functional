-- Ejercicio 4)
-- Dada la siguiente representación del lenguaje Rowbstones:
data Dir = Oeste | Este
data Exp a = Lit a | PuedeMover Dir | NroBolitas | HayBolitas | UnOp UOp (Exp a) | BinOp BOp (Exp a) (Exp a)
data UOp = No | Siguiente | Previo
data BOp = YTambien | OBien | Mas | Por
type Programa = Comando
data Comando = Mover Dir | Poner | Sacar | Skip | Repeat (Exp Int) Comando | While (Exp Bool) Comando | Sequence Comando Comando


-- a. mover :: Dir -> Tablero -> Tablero​ , que describe el tablero a partir del tablero dado donde el cabezal se movió hacia la dirección dada.
-- b. poner :: Tablero -> Tablero​ , que describe el tablero donde se agregó una bolita de la celda actual del tablero dado.
-- c. sacar :: Tablero -> Tablero​ , que describe el tablero donde se sacó una bolita de la celda actual del tablero dado.
-- d. nroBolitas :: Tablero -> Int​ , que describe la cantidad de bolitas en la celda actual del tablero dado.
-- e. hayBolitas :: Tablero -> Bool​ , que indica si hay bolitas en la celda actual del tablero dado.
-- f. puedeMover :: Dirección -> Tablero -> Bool​ , que indica si el cabezal se puede mover hacia la dirección dada en el tablero dado.
-- g. boom :: String -> a​ , que describe el resultado de realizar una operación inválida sobre un tablero.

boom :: a
boom = error "BOOM"

evalExpInt :: Exp Int -> Tablero -> Int​
-- que describe el número que resulta de evaluar la expresión dada en el tablero dado. NOTA: en caso de que la expresión no pueda computar un número,
-- debe ser considerada una operación inválida.
evalExpInt (Lit n) _ = n
evalExpInt (PuedeMover _) = boom 
evalExpInt NroBolitas = nroBolitas 
evalExpInt hayBolitas = boom 
evalExpInt (UnOp uop ea) = \t -> evalUp uop (evalExpInt e t)
evalExpInt (BinOp bop e1 e2) = \t -> evalBOp bop (evalExpInt e1 t) (evalExpInt e2 t) 

antecesor :: Int -> Int 
antecesor n = n - 1

evalUp :: UOp -> Int -> Int
evalUp No n = -n 
evalUp Siguiente = succ
evalUp Preevio = antecesor

evalBOp :: BOp -> Int -> Int -> Int
evalBOp YTambien = boom 
evalBOp OBien = boom
evalBOp Mas = (+)
evalBOp Por = (*)

evalExpBool :: Exp Bool -> Tablero -> Bool​ -- que describe el booleano que resulta de evaluar la expresión dada en el tablero dado.
evalExpBool (Lit b) = const b
evalExpBool (PuedeMover dir) = puedeMover dir
evalExpBool NroBolitas = boom 
evalExpBool HayBolitas = hayBolitas
evalExpBool (UnOp uop ea) = \t -> evalUpBool uop (evalExpBool e t)
evalExpBool (BinOp bop e1 e2) = \t -> evalBOpBool bop (evalExpBool e1 t) (evalExpBool t) 

evalUpBool :: UOp -> Bool -> Bool
evalUpBool _ = not​ 

evalBOpBool :: BOp -> Int -> Int -> Int
evalBOpBool YTambien = (&&) 
evalBOpBool OBien = (||)
evalBOpBool Mas = boom
evalBOpBool Por = boom
