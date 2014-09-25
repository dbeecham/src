import Data.Conduit
import Data.Conduit.Combinators

source :: Int -> Source IO (Double, Double)
source = sourceRandomN

sink :: Sink (Double, Double) IO Int
sink = lengthIf (\(x, y) -> sqrt (x^2 + y^2) <= 1)

montecarlo :: Int -> IO Int
montecarlo n = source n $$ sink

main = do
    p <- montecarlo 100000
