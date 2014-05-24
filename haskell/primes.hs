divisible :: Int -> Int -> Bool
divisible a b = rem a b == 0

prime :: Int -> Bool
prime n = not $ any (divisible n) $ takeWhile notTooBig [2..] where
    notTooBig x = x * x <= n

primes = filter prime [2..]
