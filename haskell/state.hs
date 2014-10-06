import Control.Applicative
import Control.Monad 
import Data.Monoid
import Data.Foldable as F

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = F.foldr (liftA2 (:)) (pure [])

data Tree a = Value a | Branch (Tree a) (Tree a) | Empty deriving Show

t, v, e :: Tree Int
t = Branch (Value 3) (Branch (Branch (Value 1) (Value 4)) (Branch (Value 3) Empty))
v = Branch Empty (Branch (Branch Empty (Value 2)) (Branch (Value 3) Empty))
e = Empty

instance Monoid (Tree a) where
    mempty = Empty
    mappend Empty t = t
    mappend t Empty = t
    mappend a b = Branch a b

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Value a) = f a
    foldMap f (Branch a b) = mappend (F.foldMap f a) (F.foldMap f b)

newtype State a = State { getState :: (Int, a) }

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | (left + n) - right < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | (right + n) - left < 4 = Just (left, right + n)
    | otherwise = Nothing


--step = \x -> [x+1, x-1] >>= (\x -> guard (x >= 0) >> return x)
step x = [x+1, x-1]
nonegatives = filter (> 0)
walk = [0] >>= (Prelude.foldr (>=>) return (replicate 20 step))
walkstat = nonegatives walk



--myreplicate :: (Monad m) => m a -> (a -> m b) -> m b
--myreplicate 
