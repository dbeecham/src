---------------
-- Version 1 --
---------------

-- Type signature
mysum :: [Int] -> Int

-- Function definition
mysum lat = if null lat then 0 else head lat + mysum (tail lat)



---------------
-- Version 2 --
---------------

-- Type signature
mysum :: [Int] -> Int

-- Function definition
mysum  = (\(lat) -> (if null lat
                        then 0
                        else ((+) (head lat) (mysum (tail lat)))))
