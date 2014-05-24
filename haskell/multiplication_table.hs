import Text.Printf

multTable n = (map (\x -> map (* x) [1..n]) [1..n])

printTable :: [Int] -> IO ()
printTable xs = do
    mapM_ (printf "%03d ") xs
    putStrLn ""

main = mapM_ printTable $ multTable 10
