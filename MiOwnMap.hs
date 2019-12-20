data Dir = Left' | Right' | Straight'
data Mapa a = 	Cofre [a]
				| Nada (Mapa a)
				| Bifurcacion [a] (Mapa a) (Mapa a)

--Dar el tipo y definir ​ foldM y ​ recM​ , que expresan los esquemas de recursión
--estructural y primitiva, respectivamente, para la estructura ​ Mapa​ .

data Objeto = Chatarra | Espada | Joya | Monedas Int | Piedra | Oro

ejemplo:: Mapa Objeto 
ejemplo = 
		Bifurcacion [Monedas 8, Oro, Chatarra, Espada] 
				(Nada (Bifurcacion [Monedas 8, Chatarra] (Cofre [Chatarra, Chatarra, Oro]) 
									  (Cofre [Espada, Piedra, Piedra]) ))
				(Nada (Bifurcacion [] (Cofre [Chatarra, Chatarra, Oro]) 
									  (Nada (Cofre [Espada, Piedra, Piedra])))) )