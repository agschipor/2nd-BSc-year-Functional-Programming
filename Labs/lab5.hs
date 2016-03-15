data List a = Vid | Cons a (List a) deriving Show

--Vid				[]
--Cons 1 Vid		1:[]
--Cons 2(Cons 1(Cons 0 Vid))
--2:(1:(0:[]))

mylength Vid = 0
mylength (Cons h t) = 1 + mylength t

--Implicit:
--Vid []
--Cons (:)

convert :: List a -> [a]
convert Vid = []
convert (Cons h t) = h:(convert t)
--convert' :: [a] -> List a
--mymap
--myfoldl
--myfoldr
--myfilter
--------------------------------
convert' :: [a] -> List a
convert' [] = Vid
convert' (h:t) = Cons h(convert'(t))
--------------------------------


--myfilter :: (a -> b) -> List a -> List a

--myfoldl :: (a -> b) -> a -> List a -> List a
--myfoldl f a Vid = Vid
--myfoldl f a (Cons h t) = myfoldl f (f a h)

mydropwhile :: (a -> Bool) -> List a -> List a
mydropwhile r Vid = Vid
mydropwhile r (Cons h t) = if r(h) == True then Cons h(mydropwhile r t) else mydropwhile r t