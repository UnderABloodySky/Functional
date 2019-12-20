capasQueCumplen3 ::(Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen3 f Prepizza = []
capasQueCumplen3 f (Capa i p) = agregarSiCorresponde f i [] ++ (capasQueCumplen3 f p)

agregarSiCorresponde::(Ingrediente -> Bool) -> Ingrediente -> [Ingrediente]
agregarSiCorresponde f i is = if (f i) then i:is else is