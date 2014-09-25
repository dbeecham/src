import System.Random

main = do
    std <- newStdGen
    print $ take 10 $ randomRs (0 :: Double, 1 :: Double) std
