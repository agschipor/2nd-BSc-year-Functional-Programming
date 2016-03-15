--Problema 1: O functie care primeste o lista de string-uri si intoarce lungimea acestora;
lgsir :: (Integral a) => [[Char]] -> [a]
lgsir [] = []
lgsir (h:t) = (foldr (\x y -> 1 + y) 0 h) : (lgsir t)

--Problema 2: O functie care calculeaza restul impartirii la 10 a sumei elementelor pare din lista data ca argument;
rest10 :: (Integral a) => [a] -> a 
rest10 [] = 0
rest10 (h:t) = foldr (+) 0 (filter (\x -> x `mod` 2 == 0) (h:t)) `mod` 10

--Problema 3: O functie care primeste la intrare o lista de cifra si intoarce numarul reprezentat de cifrele respective
lnumar :: (Integral a) => [a] -> a
lnumar [h] = if h < 10 && h > -1 then h else error "ati introdus un numar; sunt permise doar cifre"
lnumar (h:t) = if h < 10 && h > -1 then h * 10 ^ (foldr (\x y -> 1 + y) 0 t) + lnumar t else error "ati introdus un numar; sunt permise doar cifre"

--Problema 4: O functie care primeste la intrare o lista de cifre si o baza 2 <= b <= 10 si intoarce numarul reprezentat de cifrele respective in baza respectiva, numarul fiind scris invers
inverseazan :: (Integral a) => [a] -> a
inverseazan [h] = if h < 10 && h > -1 then h else error "ati introdus un numar; sunt permise doar cifre"
inverseazan (h:t) = if h < 10 && h > -1 then inverseazan t * 10 + h else error "ati introdus un numar; sunt permise doar cifre"

calculeaza_modulo :: (Integral a) => a -> a -> [a]
calculeaza_modulo 0 b = []
calculeaza_modulo n b = calculeaza_modulo (n `div` b) b ++ [n `mod` b]

conversie_baza :: (Integral a) => [a] -> a
conversie_baza [] = 0
conversie_baza (h:t) = h * 10 ^ (foldr (\x y -> 1 + y) 0 t) + conversie_baza t

converteste :: (Integral a) => [a] -> a -> a
converteste (h:t) b = if b > 1 && b < 11 then conversie_baza (calculeaza_modulo (inverseazan (h:t)) b) else error "baza invalida"

--Problema 5: O functie care inverseaza o lista
invers :: [a] -> [a]
invers[] = []
invers (h:t) = (invers t) ++ [h]