
trueLam = \x y -> x

falseLam = \x y -> y

notLam b = b falseLam trueLam
