insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs)
    | a <= x = a:x:xs
    | a > x = x:(insert' a xs)


isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = foldr insert' [] xs


occurin' :: (Eq a) => a -> [a] -> Bool
occurin' _ [] = False
occurin' y (x:xs)
    | y == x = True
    | otherwise = occurin' y xs

alloccurin' :: (Eq a) => [a] -> [a] -> Bool
alloccurin' _ [] = True
alloccurin' xs ys = and [occurin' x ys | x <- xs]
