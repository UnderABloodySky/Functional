data StyledText = Plain String | Par StyledText | Seq StyledText StyledText | Trans ATR StyledText

data ATR =  Strong | Striked | Italic

type RString = [RChar a]
data RChar a = (Char, a) | NEWLINE

renderST (Plain str) (f,g) = str2String str f 
renderST (Par st) (f,g) = renderST st (f,g)
renderST (Seq st1 st2) (f,g) = renderST st1 (f,g) ++ renderST st2 (f,g) 
renderST (Trans atr st) (f,g) = renderST st (f,g)



str2String [] _ = []
str2String (c:cs) f = f c : str2String cs f


