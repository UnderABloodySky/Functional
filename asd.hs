data StyledText = Plain String | Par StyledText | Seq StyledText StyledText | Trans ATR StyledText

data ATR =  Strong | Striked | Italic deriving Show, EQ

type RString a = [RChar a]
data RChar a = Char a | NEWLINE

renderST :: StyledText -> (Char -> RChar a, ATR -> RString a -> RString a) -> RString a
renderST (Plain str) (f,g) = str2String str f 
renderST (Par st) (f,g) = NEWLINE : renderST st (f,g)
renderST (Seq st1 st2) (f,g) = renderST st1 (f,g) ++ renderST st2 (f,g) 
renderST (Trans atr st) (f,g) = g atr (renderST st (f,g))

str2String [] _ = []
str2String (c:cs) f = f c : str2String cs f

foldST p t s pr (Plain str) = p str
foldST p t s pr (Trans atr st) = t atr (foldST p t s pr st)
foldST p t s pr (Seq st1 st2)  = s (foldST p t s pr st1)  (foldST p t s pr st2)
foldST p t s pr (Par st)  = pr (foldST p t s pr st) 

renderST' = foldST (\str (f,g) -> str2String str f)
 				   (\atr fr (f,g) -> g atr (fr (f,g)))
 				   (\fr1 fr2 (f,g)-> fr1 (f,g) ++ fr2 (f,g) )
 				   (\r (f,g) -> r (f,g) )

cantStrongChar :: StyledText -> Int
cantStrongChar (Plain str) = 0 
cantStrongChar (Par st) (f,g) = cantStrongChar st
cantStrongChar (Seq st1 st2) = cantStrongChar st1 + cantStrongChar st2 
cantStrongChar (Trans atr st) = g atr (renderST st (f,g))

normST :: StyledText -> StyledText  
normST (Plain str) = Plain str 
normST (Par st) = Par (normST st)
normST (Seq st1 st2) = armSeq (normST st1) (normST st2) 
normST (Trans atr st) = armTransf atr (normST st)

armTrans :: ATR -> StyledText -> StyledText
armTrans tr (Trans tr' st) = if tr == tr'
									then Trans tr st
									else Trans tr (Trans tr' st)
armTrans tr st = Trans tr st

armSeq :: StyledText -> StyledText -> StyledText
armSeq (Plain str1) (Plain str2) = Plain (str1++str2)
armSeq  st1 st2 = Seq st1 st2

renderST (normST (Transf atr st'))
=
renderST (armTransf atr (normST st'))
=
renderST (armTransf atr (normST st'))
=
renderSt (Transf atr (normST st'))

LemaRaro:
Prop: renderST (armTransf atr st) = renderSt (Transf atr st')
Dem:

caso 1 st = Transf atr' st' && atr == atr'

renderST (armTransf atr (Transf atr' st'))
=							(def armTransf.1)
renderSt (Trans atr st')


caso 2 st = Transf atr' st' && atr == atr'

renderST (armTransf atr (Transf atr' st'))
=							(def armTransf.1)
renderSt (Trans atr (Transf atr' st'))


renderSt (Transf atr' st')





renderSt (Transf atr' st')
=									(def)

Caso st = Transf atr' st' && atr == atr'

renderSt (armTransf atr st)
=									(def armTransf)Transf atr st
renderSt (Transf atr st')
=

Caso st = Transf atr' st' && atr != atr'

renderSt (armTransf atr st) pltr
= 
renderSt (Trans atr (Transf atr' st')) pltr
=

renderSt (Transf atr' st')
=									(def)




