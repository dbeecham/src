fact :: Int -> Int
fact n | n <= 0 = 0
       | n == 1 = 1
       | otherwise = n * fact (n - 1)

main = do
    putStrLn "Give me a number:"
    n <- getLine
    putStrLn $ show . fact. read $ n
