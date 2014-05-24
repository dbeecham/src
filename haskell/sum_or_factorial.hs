fact :: Int -> Int
fact n | n <= 0 = 0
       | n == 1 = 1
       | otherwise = n * fact (n-1)

range :: Int -> [Int]
range n | n <= 0 = []
        | otherwise = [1..n]

main = do
    putStrLn "Give me a number."
    n <- getLine
    putStrLn "factorial or sum?"
    answer <- getLine
    if answer == "factorial" then
        putStrLn $ (show . fact . read) $ n
        eshowlse putStrLn $ (show . sum . range . read) $ n
