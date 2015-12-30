import Data.Monoid
-- A monoid is an algebraic datastructure with some set of objects and an
-- associative binary operator and identity element.

-- An example, a two-tuple ZxZ is a monoid under the (strange) operation
-- (a, b) <> (c, d) = (a + c, b * d). Proof:
-- Identity:
-- (a, b) <> (0, 1) = (a + 0, b * 1) = (a, b)
-- Associativity:
-- ((a, b) <> (c, d)) <> (e, f) = (a + c, b * d) <> (e, f) = (a + c + e, b * d * f)
-- (a, b) <> ((c, d) <> (e, f)) = (a, b) <> (c + e, d * f) = (a + c + e, b * d * f)
-- This structure is in fact the product of two monoids, the addition monoid
-- and the multiplication monoid, Add x Mul.

data MyMonoid = MyMonoid Int Int

instance Monoid MyMonoid where
    mappend (MyMonoid a b) (MyMonoid c d) = MyMonoid (a + c) (b * d)
    mempty = MyMonoid 0 1

instance Show MyMonoid where
    show (MyMonoid a b) = "(" ++ show a ++ ", " ++ show b ++ ")"


-- One interesting way to look at monoids is as transformations;
-- an element a in a monoid A defines a transformation Tx -> a <> x
-- This transformation is itself a monoid under composition:
-- Ta :: x -> a <> x
-- Tb :: x -> b <> x
-- Ta <> Tb = x -> a <> b <> x
-- The identity element in this monoid is the identity transformation
-- Ti :: x -> x
-- And the transformation is associative because the underlying monoid
-- is associative.

myendo :: MyMonoid -> Endo MyMonoid
myendo m = Endo $ (<>) m


-- Using the notation a <> b as ab, then a <> a is aa, which can
-- be written as a^2. Helper function:
mpow :: Monoid m => m -> Int -> m
mpow m x = mconcat $ replicate x m


-- Now, this is interesting:
myendopow :: MyMonoid -> MyMonoid
myendopow = appEndo $ mpow (myendo (MyMonoid 1 2)) 4
-- It takes a MyMonoid, and multiplies it by (MyMonoid 1 2) 4 times.
-- It's the equation x -> y^4 <> m
