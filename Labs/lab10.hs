{-main :: IO ()
main = do
			putStr "Salut "
			getLine >>= putStr 
			getLine >>= (\x -> putStr x)
			putStr "!"
-}

{-prompt :: String -> IO String
prompt x = do 
				putStr x
				y <- getLine
				return y


main :: IO ()
main = do
			y <- prompt "Enter x: "
			x <- prompt "Enter y: "
			putStr (show ((read x :: Integer) + (read y :: Integer)))-}

prompt :: String -> IO String
prompt x = do 
				putStr x
				y <- getLine
				return y


getLines :: IO [String]
getLines = do
				y <- getLine
				if y == "quit" 
					then 
						return  [] 
					else 
						do
							z <- getLines
							return (y:z)

showLines :: [String] -> IO ()
showLines [] =  return ()
showLines (h:t) = 
					do
						putStr h
						showLines t


promptS :: IO ()						
promptS = do
				x <- getLine
				if x == "quit"
					then
						return ()
					else
						do
							putStr (show (length x))
							promptS	


showSum 0 = return "0"
showSum n = do
				y <- getLine
				z <- showSum (n - 1)
				return (show ((read z::Integer) + (read y::Integer)))

				

main :: IO ()
main = do
			x <- getLine
			putStr (showSum (read x::Integer))