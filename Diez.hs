-- Ejercicio 1) Dada la siguiente representación de un lenguaje de expresiones
-- numéricas con variables:
data NExp = Var Variable | NCte Int | Add NExp NExp | Sub NExp NExp 
            | Mul NExp NExp | Div NExp NExp  | Mod NExp NExp
type Variable = String
-- y el TAD​ Memoria ​ cuya interfaz es la siguiente:
-- enBlanco :: Memoria​ , que describe una memoria vacía.
-- cuantoVale :: Variable -> Memoria -> Maybe Int​ , que describe el número asociado a la variable dada en la memoria dada.
-- recordar :: Variable -> Int -> Memoria -> Memoria​ , que la memoria resultante de asociar el número dado a la variable dada en la
-- memoria dada.
-- variables :: Memoria -> [Variable]​ , que describe las variables que la memoria recuerda.
data Memoria = Mem

cuantoVale :: Variable -> Memoria -> Maybe Int
cuantoVale v m = undefined

recordar :: Variable -> Int -> Memoria -> Memoria
recordar v n m = undefined

foldNExp :: (Variable -> b) -> (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> NExp -> b
foldNExp v b a s m d md (Var var) = v var
foldNExp v b a s m d md (NCte n) = b n 
foldNExp v b a s m d md (Add ne1 ne2) = a (foldNExp v b a s m d md ne1) (foldNExp v b a s m d md ne2)
foldNExp v b a s m d md (Sub ne1 ne2) = s (foldNExp v b a s m d md ne1) (foldNExp v b a s m d md ne2)
foldNExp v b a s m d md (Mul ne1 ne2) = m (foldNExp v b a s m d md ne1) (foldNExp v b a s m d md ne2)
foldNExp v b a s m d md (Div ne1 ne2) = d (foldNExp v b a s m d md ne1) (foldNExp v b a s m d md ne2)
foldNExp v b a s m d md (Mod ne1 ne2) = md (foldNExp v b a s m d md ne1) (foldNExp v b a s m d md ne2)  

recNExp :: (Variable -> b) -> (Int -> b) -> (NExp -> b -> NExp -> b -> b) -> (NExp -> b -> NExp ->  b -> b) -> (NExp -> b -> NExp ->  b -> b) -> (NExp -> b -> NExp ->  b -> b) -> (NExp -> b -> NExp ->  b -> b) -> NExp -> b
recNExp v b a s m d md (Var var) = v var
recNExp v b a s m d md (NCte n) = b n 
recNExp v b a s m d md (Add ne1 ne2) = a ne1 (recNExp v b a s m d md ne1) ne2 (recNExp v b a s m d md ne2)
recNExp v b a s m d md (Sub ne1 ne2) = s ne1 (recNExp v b a s m d md ne1) ne2 (recNExp v b a s m d md ne2)
recNExp v b a s m d md (Mul ne1 ne2) = m ne1 (recNExp v b a s m d md ne1) ne2 (recNExp v b a s m d md ne2)
recNExp v b a s m d md (Div ne1 ne2) = d ne1 (recNExp v b a s m d md ne1) ne2 (recNExp v b a s m d md ne2)
recNExp v b a s m d md (Mod ne1 ne2) = md ne1 (recNExp v b a s m d md ne1) ne2 (recNExp v b a s m d md ne2)  


-- implementar las siguientes funciones:
evalNExp :: NExp -> Memoria -> Int
-- que describe el número resultante de evaluar la expresión dada a partir de la memoria dada.
evalNExp (Var var) mem 	   = case (cuantoVale var mem) of
									Nothing -> error "Undefined var"
									Just n -> n
evalNExp (NCte n) mem  	   = n
evalNExp (Add ne1 ne2) mem = evalNExp ne1 mem + evalNExp ne2 mem   
evalNExp (Sub ne1 ne2) mem = evalNExp ne1 mem - evalNExp ne2 mem 
evalNExp (Mul ne1 ne2) mem = evalNExp ne1 mem * evalNExp ne2 mem 
evalNExp (Div ne1 ne2) mem = div (evalNExp ne1 mem)  (evalNExp ne2 mem) 
evalNExp (Mod ne1 ne2) mem = mod (evalNExp ne1 mem)  (evalNExp ne2 mem)

constantFoldingNE :: NExp -> NExp
-- que describe una expresión con el mismo significado que la dada, pero simplificada y
-- reemplazando las subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente
-- simbólica.
constantFoldingNE (Var var)		= Var var 
constantFoldingNE (NCte n) 		= NCte n 
constantFoldingNE (Add ne1 ne2) = armAdd (constantFoldingNE ne1) (constantFoldingNE ne2) 
constantFoldingNE (Sub ne1 ne2) = armSub (constantFoldingNE ne1) (constantFoldingNE ne2) 
constantFoldingNE (Mul ne1 ne2) = armMul (constantFoldingNE ne1) (constantFoldingNE ne2) 
constantFoldingNE (Div ne1 ne2) = armDiv (constantFoldingNE ne1) (constantFoldingNE ne2)
constantFoldingNE (Mod ne1 ne2) = Mod (constantFoldingNE ne1) (constantFoldingNE ne2)

armAdd :: NExp -> NExp -> NExp
armAdd (NCte 0) e2 = e2
armAdd  e1 (NCte 0) = e1
armAdd (NCte n) (NCte m) = NCte (n+m)
armAdd  e1 e2 = Add e1 e2 

armSub :: NExp -> NExp -> NExp
armSub  e1 (NCte 0) = e1
armSub  (NCte n) (NCte m) = NCte (n-m)
armSub  e1 e2 = Sub e1 e2 

armMul :: NExp -> NExp -> NExp
armMul (NCte 0) _ = NCte 0
armMul _ (NCte 0) = NCte 0
armMul (NCte 1) e2 = e2
armMul e1 (NCte 1) = e1
armMul (NCte n) (NCte m) = NCte (n*m)
armMul e1 e2 = Mul e1 e2

armDiv :: NExp -> NExp -> NExp
armDiv  e1 (NCte 1) = e1
armDiv  e1 (NCte 0) = error "Div by zero"
armDiv  (NCte n) (NCte m) = NCte (div n m)
armDiv  e1 e2 = Div e1 e2

constantFoldingNE' :: NExp -> NExp 
constantFoldingNE' = foldNExp Var NCte armAdd armSub armMul armDiv Mod

evalNExp' :: NExp -> Memoria -> Int
evalNExp' = foldNExp (\var mem -> case (cuantoVale var mem) of
 									Nothing -> error "Undefined var"
 									Just n -> n)
					 (\n mem -> n)
					 (\f1 f2 mem -> f1 mem + f2 mem)
					 (\f1 f2 mem -> f1 mem - f2 mem)
					 (\f1 f2 mem -> f1 mem * f2 mem)
					 (\f1 f2 mem -> div (f1 mem) (f2 mem))
					 (\f1 f2 mem -> mod (f1 mem) (f2 mem))

-- Prop: evalNExp . constantFoldingNE ​ = ​ evalNExp
-- Dem: Por principio de extensionalidad:
--			V e :: NExp . (evalNExp . constantFoldingNE) ​e = ​ evalNExp e
-- Dem: Por principio de extensionalidad:
--			V e :: NExp . V mem :: Memoria . (evalNExp . constantFoldingNE) ​e mem = ​ evalNExp e mem
--		Por definicion (.)
--			V e :: NExp . evalNExp (constantFoldingNE ​e) mem = ​ evalNExp e mem 
-- 	    Por principio de induccion estructural en la estructura de e (e :: NExp)

-- Caso base: e = Var x
--		evalNExp (constantFoldingNE ​(Var x))  mem = ​ evalNExp (Var x) mem 
-- evalNExp (constantFoldingNE ​(Var x)) mem 		
-- =						(def constantFldingNE.1)
-- evalNExp (Var x) mem 

-- Caso base: e = NCte n
--		evalNExp (constantFoldingNE ​(NCte n)) mem = ​ evalNExp (NCte n) mem 

-- evalNExp (constantFoldingNE ​(NCte n)) mem 		
-- =						(def constantFoldingNE.2)
-- evalNExp (NCte n) mem 

-- Caso Inductivo: e = Add ne1 ne2
--		HI1: evalNExp (constantFoldingNE ​ne1) mem = ​ evalNExp ne1 mem 
--		HI2: evalNExp (constantFoldingNE ​ne2) mem  = ​ evalNExp ne2 mem 
--		TI: evalNExp (constantFoldingNE ​(Add ne1 ne2)) mem = ​ evalNExp (Add ne1 ne2) mem  

-- evalNExp (constantFoldingNE ​(Add ne1 ne2)) mem 
-- =						(def constantFoldingNE .3)
-- evalNExp (armAdd (constantFoldingNE ne1) (constantFoldingNE ne2)) mem 
-- =						(lemaAdd)
-- evalNExp (constantFoldingNE ne1) mem + evalNExp (constantFoldingNE ne2) mem 
-- =						(HI1)
-- evalNExp ne1 mem  + evalNExp (constantFoldingNE ne2) mem 
-- =						(HI2)
-- evalNExp ne1 mem + evalNExp ne2 mem
-------- <<<< Demas casos como este. >>>> -------- 

-- lemaAdd
-- Prop: evalNExp (armAdd ne1 ne2) mem = evalNExp ne1 mem + evalNExp ne2 mem
-- Dem: 

-- ne1 = NCte 0
-- 		evalNExp (armAdd (NCte 0) ne2) mem = evalNExp (NCte 0) mem + evalNExp ne2 mem

-- evalNExp (armAdd (NCte 0) ne2) mem
-- =						(def armAdd.1)
-- evalNExp ne2 mem

-- evalNExp (NCte 0) mem + evalNExp ne2 mem
-- =						(def evalNExp.2)
-- 0 + evalNExp ne2 mem
-- =						(Aritmetica/Identidad)
-- evalNExp ne2 mem

-- ne2 = NCte 0
--		evalNExp (armAdd ne1 (NCte 0)) mem = evalNExp ne1 mem + evalNExp (NCte 0) mem

-- evalNExp (armAdd ne1 (NCte 0)) mem 
-- =						(def armAdd.2)
-- evalNExp ne1 mem 

-- evalNExp ne1 mem + evalNExp (NCte 0) mem
-- =						(def evalNExp.2)
-- evalNExp ne1 mem + 0
-- =						(Aritmetica/Identidad)
-- evalNExp ne1 mem


-- ne1 = NCte n && ne2 = NCte m (donde n != 0 & m  != 0)
--		evalNExp (armAdd (NCte n) (NCte m)) mem = evalNExp (NCte n) mem + evalNExp (NCte m) mem  

-- evalNExp (armAdd (NCte n) (NCte m)) mem
-- =						(def armAdd.3)
-- evalNExp (Cte (n+m))
-- =						(def evalNExp.2)
-- n + m

-- evalNExp (NCte n) mem + evalNExp (NCte m) mem  
-- =						(def evalNExp.2)
-- n + evalNExp (NCte m) mem
-- =						(def evalNExp.2)
-- n + m

-- ne1 != NCte _ && ne2 = NCte _
--		evalNExp (armAdd ne1 ne2) mem = evalNExp ne1 mem + evalNExp ne2 mem  

-- evalNExp (armAdd ne1 ne2) mem
-- = 						(def armAdd.4)
-- evalNExp (Add ne1 ne2) mem
-- = 						(def evalNExp.3)
-- evalNExp ne1 mem + evalNExp ne2 mem


-- Ejercicio 2) Dada la siguiente representación de un lenguaje de expresiones booleanas:

data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | RelOp ROp NExp NExp
data ROp = Equal | NotEqual | Greater | GreaterEqual | Lower | LowerEqual

foldBExp :: (Bool -> a) -> (a -> a) -> (a ->  a -> a) -> (a ->  a -> a) -> (ROp -> NExp -> NExp -> a) -> BExp -> a
foldBExp b n a o r (BCte bool) = b bool
foldBExp b n a o r (Not be) = n (foldBExp b n a o r be)
foldBExp b n a o r (And be1 be2) = a (foldBExp b n a o r be1) (foldBExp b n a o r be2) 
foldBExp b n a o r (Or be1 be2) = o (foldBExp b n a o r be1) (foldBExp b n a o r be2) 
foldBExp b n a o r (RelOp rOp ne1 ne2) = r rOp ne1 ne2

evalBExp :: BExp -> Memoria -> Bool​
-- que describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.
evalBExp (BCte b) _              = b 
evalBExp (Not be) mem      		 = not  (evalBExp be mem)
evalBExp (And be1 be2) mem       = (&&) (evalBExp be1 mem) (evalBExp be2 mem)
evalBExp (Or be1 be2) mem  		 = (||) (evalBExp be1 mem) (evalBExp be2 mem) 
evalBExp (RelOp rOp ne1 ne2) mem = evalROp rOp (evalNExp ne1 mem) (evalNExp ne2 mem) 

evalROp :: ROp -> Int -> Int -> Bool
evalROp Equal 		 = (==)
evalROp NotEqual 	 = (/=)
evalROp Greater      = (>) 
evalROp GreaterEqual = (>=)
evalROp Lower        = (<)
evalROp LowerEqual   = (<=)

evalBExp' :: BExp -> Memoria -> Bool
evalBExp' = foldBExp const
                     (\r -> not . r)
                     (\r1 r2 mem -> (&&) (r1 mem) (r2 mem))
					 (\r1 r2 mem -> (||) (r1 mem) (r2 mem))
					 (\rOp ne1 ne2 mem -> evalROp rOp (evalNExp ne1 mem) (evalNExp ne2 mem))

constanFoldingBE :: BExp -> BExp
-- que describe una expresión con el mismo significado que la dada, pero reemplazando
-- las subexpresiones que no dependan de la memoria por su expresión
-- más sencilla. La resolución debe ser exclusivamente simbólica .
constanFoldingBE (BCte b)            = BCte b 
constanFoldingBE (Not be)      		 = armNot  (constanFoldingBE be)
constanFoldingBE (And be1 be2)       = armAnd (constanFoldingBE be1) (constanFoldingBE be2)
constanFoldingBE (Or be1 be2)  		 = armOr (constanFoldingBE be1) (constanFoldingBE be2) 
constanFoldingBE (RelOp rOp ne1 ne2) = RelOp rOp (constantFoldingNE ne1) (constantFoldingNE ne2) 

constanFoldingBE' ::  BExp -> BExp
constanFoldingBE' = foldBExp BCte 
                             armNot 
                             armAnd 
                             armOr 
                             (\rOp ne1 ne2 -> RelOp rOp (constantFoldingNE ne1) (constantFoldingNE ne2)) 

armNot :: BExp -> BExp
armNot (BCte False) = BCte True
armNot (BCte True)  = BCte False
armNot e 			= Not e	

armAnd :: BExp -> BExp -> BExp
armAnd (BCte True) be2  = be2 		 -- Identidad
armAnd be1 (BCte True)  = be1
armAnd (BCte False) be2 = BCte False -- Dominacion
armAnd be1 (BCte False) = BCte False
armAnd be1 be2          = And be1 be2

armOr :: BExp -> BExp -> BExp
armOr (BCte True) be2  = BCte True  -- Dominacion
armOr be1 (BCte True)  = BCte True
armOr (BCte False) be2 = be2 		-- Identidad
armOr be1 (BCte False) = be1
armOr be1 be2          = And be1 be2


-- Prop: evalBExp . constantFoldingBE ​ = ​ evalBExp
-- Dem:  Por principio de extensionalidad
--				V e :: NExp . (evalBExp . constantFoldingBE) e​ = ​ evalBExp e
--		 Por principio de extensionalidad
--				V mem :: Memoria . V e :: BExp . (evalBExp . constantFoldingBE) e​ mem = ​ evalBExp e mem 
--		 Por defininicion (.)
--				V mem :: Memoria . V e :: BExp . evalBExp (constantFoldingBE e)​ mem = ​ evalBExp e mem 
--		 Por principio de induccion estructural en la estructura de e (e :: BExp)

-- Caso base: e = BCte b
-- 				evalBExp (constantFoldingBE (BCte b))​ mem = ​ evalBExp (BCte b) mem 

-- evalBExp (constantFoldingBE (BCte b))​ mem 
-- =							(def constantFoldingBE.1)
-- evalBExp (BCte b) mem
-- =							(def evalBExp.1)
-- b

-- Caso inductivo: e = Not be
-- 				Hi: evalBExp (constantFoldingBE be)​ mem = ​ evalBExp be mem 
-- 				Ti: evalBExp (constantFoldingBE (Not be))​ mem = ​ evalBExp (Not be) mem 

-- evalBExp (constantFoldingBE (Not be))​ mem 
-- =							(def constantFoldingBE.2)
-- evalBExp (armNot (constantFoldingBE be))​ mem 
-- =							(lemaNot)
-- not (evalBExp (constantFoldingBE be) mem)
-- =							(HI)
-- not evalBExp be mem
-- =							(def evalBExp.2)
-- evalBExp (Not be) mem 

-- Caso inductivo: e = And be1 be2
-- 				HI1: evalBExp (constantFoldingBE be1)​ mem = ​ evalBExp be1 mem
-- 				HI2: evalBExp (constantFoldingBE be2)​ mem = ​ evalBExp be2 mem
-- 				TI: evalBExp (constantFoldingBE (And be1 be2))​ mem = ​evalBExp (And be1 be2) mem 

-- evalBExp (constantFoldingBE (And be1 be2))​ mem
-- =							(def constantFoldingBE.3)
-- evalBExp (armAnd (constantFoldingBE be1) (constantFoldingBE be2)) mem
-- = 							(lemaAND)
-- (&&) (evalBExp (constantFoldingBE be1) mem) (evalBExp (constantFoldingBE be2) mem) 		
-- =							(HI1)
-- (&&) (evalBExp be1 mem) (evalBExp be2 mem) 		
-- =							(HI1)
-- (&&) (evalBExp be1 mem) (evalBExp be2 mem) 		
-- =							(def evalBExp.3)
-- evalBExp (And be1 be2) mem

-- Caso base: e = RelOp rOp ne1 ne2
-- 				evalBExp (constantFoldingBE (RelOp rOp ne1 ne2)​ mem = ​ evalBExp (RelOp rOp ne1 ne2) mem 
				
-- evalBExp (constantFoldingBE (RelOp rOp ne1 ne2) mem ​ 
-- =							(def constantFoldingBE.4)
-- evalBExp (RelOp rOp (constantFoldingBE ne1) (constantFoldingBE ne2)) mem ​ 
-- =							(def evalBExp.4)
-- evalROp rOp (evalNExp (constantFoldingNE ne1) mem) (evalNExp (constantFoldingNE ne2) mem) 
-- =							(def (.))
-- evalROp rOp ((evalNExp.constantFoldingNE) ne1 mem) (evalNExp (constantFoldingNE ne2) mem) 
-- =							(def (.))
-- evalROp rOp ((evalNExp.constantFoldingNE) ne1 mem) ((evalNExp.constantFoldingNE) ne2 mem) 
-- =							(Demostracion del ejercicio 1)
-- evalROp rOp (evalNExp ne1 mem) (evalNExp ne2 mem)

-- evalBExp (RelOp rOp ne1 ne2) mem 	
-- =							(def evalBExp.4)
-- evalROp rOp (evalNExp ne1 mem) (evalNExp ne2 mem) 

-- lemaAND
-- Prop: evalBExp (armAnd be1 be2) mem = (&&) (evalBExp be1 mem) (evalBExp be2 mem) 		
-- Dem:

-- Caso 1 - be1 = BCte True
-- 			evalBExp (armAnd (BCte True) be2) mem = (&&) (evalBExp (BCte True) mem) (evalBExp be2 mem) 

-- evalBExp (armAnd (BCte True) be2) mem
-- =							(def armAnd.1)
-- evalBExp be2

-- (&&) (evalBExp (BCte True) mem) (evalBExp be2 mem) 
-- =							(def evalBExp.1)
-- (&&) True (evalBExp be2 mem)
-- =							(Identidad)
-- evalBExp be2 mem

-- Caso 2 - be2 = BCte True
-- 			evalBExp (armAnd be1 (BCte True)) mem = (&&) (evalBExp be1 mem) (evalBExp (BCte True) mem) 

-- evalBExp (armAnd be1 (BCte True)) mem
-- =							(def armAnd.1)
-- evalBExp be1

-- (&&) (evalBExp be1 mem) (evalBExp (BCte True) mem) 
-- =							(def evalBExp.1)
-- (&&) (evalBExp be1 mem) True
-- =							(Identidad)
-- evalBExp be1 mem

-- Caso 3 - be1 = BCte False
-- 			evalBExp (armAnd (BCte False) be2) mem = (&&) (evalBExp (BCte False) mem) (evalBExp be2 mem) 

-- evalBExp (armAnd (BCte False) be2) mem
-- =							(def armAnd.1)
-- evalBExp (BCte False)
-- =							(def evalBExp.1)
-- False

-- (&&) (evalBExp (BCte False) mem) (evalBExp be2 mem) 
-- =							(def evalBExp.1)
-- (&&) False (evalBExp be2 mem)
-- =							(Dominacion)
-- False

-- Caso 4 -be2 = BCte False
-- 			evalBExp (armAnd be1 (BCte False)) mem = (&&) (evalBExp be1 mem) (evalBExp (BCte False) mem) 

-- evalBExp (armAnd be1 (BCte False)) mem
-- =							(def armAnd.1)
-- evalBExp (BCte False)
-- =							(def evalBExp.1)
-- False

-- (&&) (evalBExp be1 mem) (evalBExp (BCte True) mem) 
-- =							(def evalBExp.1)
-- (&&) (evalBExp be1 mem) False
-- =							(Dominacion)
-- False

-- Caso 5 - be1 != BCte True && be1 != BCte False && be2 != BCte True && be2 != BCte False
-- 			evalBExp (armAnd be1 be2) mem = (&&) (evalBExp be1 mem) (evalBExp be2 mem) 

-- evalBExp (armAnd be1 be2) mem
-- =							(def armAnd.1)
-- evalBExp (And be1 be2) men
-- =							(def evalBExp.3)
-- (&&) (evalBExp be1 mem) (evalBExp be2 mem)


-- Ejercicio 3)
-- Dada la siguiente representación de un lenguaje imperativo simple:

data Programa = P Bloque
type Bloque = [Comando]
type Nombre = String
data Comando = Skip | Assign Nombre NExp | If BExp Bloque Bloque | While BExp Bloque

-- implementar las siguientes funciones:
evalPrograma :: Programa -> Memoria -> Memoria​
-- que describe la memoria resultante de evaluar el programa dado a partir
evalPrograma (P bloque) = evalBloque bloque

evalBloque :: Bloque -> Memoria -> Memoria​
-- que describe la memoria resultante de evaluar el bloque dado a partir de la memoria dada.
evalBloque [] = id
evalBloque (c:cs) = \mem -> let mem' = evalComando c mem
						    in evalBloque cs mem'

evalComando :: Comando -> Memoria -> Memoria​
-- que describe la memoria resultante de evaluar el comando dado a partir de la memoria dada.
-- recordar :: Variable -> Int -> Memoria -> Memoria
evalComando Skip                = id 
evalComando (Assign n ne)       = \mem -> recordar n (evalNExp ne mem) mem 
evalComando (If bExp blq1 blq2) = \mem -> if (evalBExp bExp mem)
												then evalBloque blq1 mem
												else evalBloque blq2 mem  
evalComando (While bExp blq)    = evalComando (If bExp (blq ++ [While bExp blq]) ([Skip]))

optimize :: Programa -> Programa
-- que describe un programa con el mismo significado que el dado, pero aplicando constant folding sobre las expresiones y descartando los comandos
-- que no serán ejecutados.de la memoria dada.
optimize (P bloque) = P (optimizeBloque bloque)

optimizeBloque :: Bloque -> Bloque
optimizeBloque [] = []
optimizeBloque (c:cs) = optimizeComand c : optimizeBloque cs

optimizeComand :: Comando -> Comando
optimizeComand Skip                = Skip 
optimizeComand (Assign n ne)       = Assign n (constantFoldingNE ne) 
optimizeComand (If bExp blq1 blq2) = If (constanFoldingBE bExp) (optimizeBloque blq1) (optimizeBloque blq2) 
optimizeComand (While bExp blq)    = While (constanFoldingBE bExp) (optimizeBloque blq)


-- Prop: evalPrograma . optimize ​ = ​ evalPrograma
-- Dem: Por principio de extensionalidad:
--				V p :: Programa . (evalPrograma . optimize) ​p = ​ evalPrograma p
--		Por principio de extensionalidad:
--				V mem :: Memoria . V p :: Programa . (evalPrograma . optimize) ​p mem = ​ evalPrograma p mem
--		Por definicion del (.)
--				V p :: Programa . evalPrograma (optimize ​p) mem = ​ evalPrograma p mem
-- 				evalPrograma (optimize ​(P bloque)) mem = ​ evalPrograma (P bloque) mem
-- evalPrograma (optimize ​(P [])) 
-- =						(def optimize)
-- evalPrograma (P (optimizeBloque []))
-- =						(def. optimizeBloque.1)
-- evalPrograma (P []) mem


-- Caso Inductivo: p = P (c:cs)
--				HI: evalPrograma (optimize ​(P cs)) mem = ​ evalPrograma (P cs) mem
--				TI: evalPrograma (optimize ​(P (c:cs))) mem = ​ evalPrograma (P (c:cs)) mem

-- evalPrograma (optimize ​(P (c:cs))) mem 
-- = 						(def optimize)
-- evalPrograma (P (optimizeBloque (c:cs))) mem
-- =						(def optimizeBloque.2)
-- evalPrograma (P (optimizeComand c : optimizeBloque cs)) mem
-- =						(def evalPrograma)
-- evalBloque (optimizeComand c : optimizeBloque cs) mem
-- =						(def evalBloque.2)
-- (\c cs mem -> let mem' = evalComando c mem
--						    in evalBloque cs mem') (optimizeComand c) (optimizeBloque cs) mem
-- =						(BETA) 
-- evalComando (optimizeComand c) mem : evalBloque cs mem'

-- evalPrograma (P (c:cs)) mem
-- =						(def evalPrograma)
-- evalBloque (c:cs) mem
-- =						(def evalBloque.2)
-- (\c cs mem -> let mem' = evalComando c mem
--						    in evalBloque cs mem') c cs mem
-- =						(BETA)
-- evalComando c mem : evalBloque cs mem'


-- Ejercicio 4)
-- Dada la siguiente representación del lenguaje Rowbstones:
data Dir = Oeste | Este
data Exp a = Lit a | PuedeMover Dir | NroBolitas | HayBolitas | UnOp UOp (Exp a) | BinOp BOp (Exp a) (Exp a)
data UOp = No | Siguiente | Previo
data BOp = YTambien | OBien | Mas | Por
type Programita = Comando
data Comandito = Mover Dir | Poner | Sacar | Skipy | Repeat (Exp Int) Comando | Whiley (Exp Bool) Comando | Sequence Comando Comando