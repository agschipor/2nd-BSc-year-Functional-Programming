maxim[h] = h
maxim(h:t) = max2 h (maxim t)

max2 x y = if x > y then x else y

inc10 x = x + 10

aduna = \x y -> x + y

doublef = \x -> 2 * x

triple = \x -> 3 * x

parcurge f [] = []
parcurge f (h:t) = (f h) : (parcurge f t)

rest10 = parcurge (\x -> mod x 10)

filtrare f [] = []
filtrare f (h:t) = if f h then h:(filtrare f t)
				   else filtrare f t

pastreazapar = filtrare (\x -> mod x 2 == 0)

sumarizare f i [] = i
sumarizare f i (h:t) = (f h)  (sumarizare f i t)

sumalista = sumarizare (+) 0
prodlista = sumarizare (*) 1
lenglista = sumarizare (\x y -> 1 + y) 0