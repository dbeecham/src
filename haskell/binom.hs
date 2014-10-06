import Math.Combinatorics.Exact.Binomial

dist :: Int -> [Double]
dist x = map ((/ 2^x) . fromIntegral . choose x) [0..x]
