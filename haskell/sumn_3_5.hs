import Control.Monad

range :: Int -> [Int]
range n | n < 1 = []
        | otherwise = [1..n]

divides :: Int -> Int -> Bool
divides a b = rem a b == 0

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

sum3or5 :: Int -> Int
sum3or5 = sum . filter (divides 3 <||> divides 5) . range

main = do
    putStrLn "Give me a number"
    n <- getLine
    putStrLn $ show . sum3or5 . read $ n
