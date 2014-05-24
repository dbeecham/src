-- ## DEFINITION ##
-- import Control.Monad.Fix
-- fix :: (a -> a) -> a
-- fix = let x = f x in x


-- Example:
-- fix (2 +)
-- (2 + (fix (2 +)))
-- (2 + (2 + (fix (2 +))))
-- (2 + (2 + (2 + (fix (2 +)))))

-- fix (const "hello")
-- (const "hello" (fix (const "hello")))
-- "hello"

-- fix (1:)
-- (1:(fix (1:)))
-- (1:(1:(fix (1:))))
-- (1:(1:(1:(fix (1:)))))
--
-- fix reverse
-- (reverse (fix reverse))
-- (reverse (reverse (fix reverse)))
-- (reverse (reverse (reverse (fix reverse))))
-- 
-- fix fix
-- (fix (fix fix))
-- (fix (fix (fix fix)))
-- (fix (fix (fix (fix fix))))
--
-- fix id
-- (id (fix id))
-- (id (id (fix id)))
-- (id (id (id (fix id))))
