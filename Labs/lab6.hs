f x = 1 + f x

f' x = x : (f' (x + 1))

l = 1 : l

--fib = 0:1:

--l = [[1], [1,1], [2,1], [1,2,1,1], [1,1,1,2,2,1]]

len [] = 0
len (h:t) = 1 + (len t)

equ [] = []
equ [x] = [x]
equ (h:t) = if h == (head t) then h:(equ t) else [h]

el [] = []
el (h:t) = len(equ(h:t)):[h]++(el (drop (len(equ(h:t))) (h:t)))

--list = [1]:[el [1]]++[el (head (drop 1 list))
--list = [1]:[el (head (list))]
--list = [1]:

--par = 

factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n - 1)

lf x = (factorial x):(lf (x+1))

faclist = lf 0