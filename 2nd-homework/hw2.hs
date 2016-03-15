deleteFirst :: (Eq a) => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (h:t) | a == h = t
					| otherwise = h : deleteFirst a t
--1--
--a--
insertElement :: (Ord a) => a -> [a] -> [a]
insertElement p [] = [p]
insertElement p xs = left ++ (p:right)
					 where
					 	right = filter (>= p) xs
					 	left = filter (< p) xs

insertElement2 :: (Ord a) => a -> [a] -> [a]
insertElement2 p [] = [p]
insertElement2 p (h:t) = if (p <= h) then [p] ++ (h:t) else h : insertElement2 p t
--b--
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (h:t) = insertElement h (sort t)

--2--
--a--
isDecreasing :: (Ord a) => [a] -> Bool
isDecreasing [] = False
isDecreasing [x] = True
isDecreasing (x:xs) = if x < head xs then False else isDecreasing xs
--b--
suffixDecreasing :: (Ord a) => [a] -> [a]
suffixDecreasing [] = []
suffixDecreasing (h:t) = if (isDecreasing (h:t)) then (h:t) else suffixDecreasing t 
--c--
whereDecreasing :: (Ord a) => [a] -> a
whereDecreasing [] = error "nu este posibil"
whereDecreasing (h:t) = if isDecreasing(h:t) then error "nu exista" else if isDecreasing(t) then h else whereDecreasing t
--d--
preffixDecreasing :: (Ord a) => [a] -> [a]
preffixDecreasing [] = []
preffixDecreasing (h:t) = if isDecreasing(t) == True then [] else h : preffixDecreasing t

--3--
--a--
smallest :: (Ord a) => [a] -> a
smallest [] = error "lista vida"
smallest [x] = x
smallest (x:xs) = if x < head xs then 
				  	smallest(x:(drop 1 xs)) 
				  else 
				  	smallest(xs)

smallest2 :: (Ord a) => [a] -> a
smallest2 [] = error "lista vida"
smallest2 (h:t) = head (sort (h:t))

--b--
smallestGreaterThan :: (Ord a) => [a] -> a -> a
smallestGreaterThan [] p = error "lista vida"
smallestGreaterThan xs p = smallest (filter (>p) (xs))

--c--
remove :: (Eq a) => [a] -> a -> [a]
remove xs p = filter (/=p) xs

--4-
nextPermutation :: (Ord a) => [a] -> [a]
nextPermutation xs = if isDecreasing xs == False then preffixDecreasing(xs) ++ [smallestGreaterThan (suffixDecreasing xs) (whereDecreasing (xs))] ++ insertElement (whereDecreasing xs) (sort (remove (suffixDecreasing xs) (smallestGreaterThan (suffixDecreasing xs) (whereDecreasing xs)))) else xs

--5--
--a--
isLastPermutation :: (Ord a) => [a] -> Bool
isLastPermutation xs = isDecreasing xs
--b--
permutations' :: (Ord a) => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = if isLastPermutation xs == False then [xs] ++ permutations' (nextPermutation xs) else [xs]
--c--
permutations :: (Integral a) => a -> [[a]]
permutations n = permutations' [1..n]

