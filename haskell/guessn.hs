import System.Random

guessMe :: Int -> IO ()
guessMe x = do
    g0 <- getLine
    let g1 = read g0
    if g1 == x then putStrLn "yes"
        else if g1 > x then putStrLn "Too high."
            else putStrLn "Too low."

main = do
    roll <- getStdRandom (randomR(1,100))
    x <- roll
    guessMe x
