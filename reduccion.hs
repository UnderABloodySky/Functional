--foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
--foldT b fn EmptyT = b
--foldT​ b fn (NodeT x t1 t2) = fn x (foldT​ b fn t1) (foldT​ b fn t2)



todosLosCaminos			NodeT 2 (NodeT 4 EmptyT EmptyT) 
					  			(NodeT 6 EmptyT EmptyT)

foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))	(NodeT 2 (NodeT 4 EmptyT EmptyT) 
					  									(NodeT 6 EmptyT EmptyT))

(\x l1 l2 -> agregar x (l1 ++ l2)) 2 
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 4 EmptyT EmptyT)) 
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT))  

agregar 2(
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 4 EmptyT EmptyT)) 
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT)))

agregar 2(
		((\x l1 l2 -> agregar x (l1 ++ l2)) 4 (foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) EmptyT) (foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) EmptyT)) 
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT)))

agregar 2(
		(agregar 4
		((foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) EmptyT) (foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) EmptyT)))
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT)))

agregar 2(
		(agregar 4
		[[]] (foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) EmptyT)))
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT)))

agregar 2(
		(agregar 4
		[[]] [[]])
		(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2)) (NodeT 6 EmptyT EmptyT)))

agregar 2(
		(agregar 4
		[[]] [[]])
		((\x l1 l2 -> agregar x (l1 ++ l2)) 6 
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))) EmptyT)
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))) EmptyT))

agregar 2(
		(agregar 4
		[[]] [[]])
		(agregar 6
			((foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))) EmptyT)
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))) EmptyT)))

agregar 2(
		(agregar 4
		[[]] [[]])
		(agregar 6
			[[]] 
			(foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))) EmptyT)))

agregar 2(
		(agregar 4
		[[]] [[]])
		(agregar 6
			[[]] [[]])))

agregar 2(
		(
		[[2, 4]] [[2, 4]])
			[[2, 6]] [[2, 6]])))

