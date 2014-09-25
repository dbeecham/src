import System.Random
import Data.List

rnds :: StdGen -> [Double]
rnds = randoms

hit :: Double -> Bool
hit = (< 0.5)

main = do
    seed <- newStdGen
    let all = 3000000
    let hits = foldl' (\cnt a -> if (hit a) then (cnt + 1) else cnt) 0 (take all (rnds seed))
    print ((toRational hits) / (toRational all))
