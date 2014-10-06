data Tree a = Value a | Branch (Tree a) (Tree a) | Empty deriving Show

instance Functor Tree where
    fmap f (Value a) = Value $ f a
    fmap _ Empty = Empty
    fmap f (Branch a b) = Branch (fmap f a) (fmap f b)
