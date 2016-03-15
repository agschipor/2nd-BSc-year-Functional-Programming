--operatiile myand mynot myor

data MyBool = MyTrue | MyFalse

myand :: MyBool -> MyBool -> MyBool
myand MyTrue MyTrue = MyTrue
myand x y = MyFalse

myor :: MyBool -> MyBool -> MyBool
myor MyFalse MyFalse = MyFalse
myor x y = MyTrue

mynot :: MyBool -> MyBool
mynot MyTrue = MyFalse
mynot x = MyTrue

data MaybeInt = MyError | MyInt Int deriving Show

mydiv :: Int -> Int -> MaybeInt
mydiv a 0 = MyError
myDiv a b = MyInt(a `div` b)

f :: MaybeInt -> MaybeInt -> MaybeInt
f MyError MyError = MyError
f MyError (MyInt a) = MyError
f (MyInt a) MyError = MyError
f (MyInt a) (MyInt 0) = MyError
f (MyInt a) (MyInt b) = MyInt (div a b)

data MaybeString = MyError' | MyString String deriving Show

data Maybe' a = Nimic | Ceva a deriving Show

g :: String -> Maybe' String
g "" = Nimic
g (h:t) = (Ceva t)

sir :: Maybe' String -> Maybe' String
sir Nimic = Nimic
sir (Ceva "") = Nimic
sir (Ceva (h:t)) = (Ceva t)

rezolva :: Double -> Double -> Maybe Double
rezolva 0 b = Nothing
rezolva a b = (Just ((-1 * b)/a))