main = return 3

data Tree a = Empty | Node a [Tree a] deriving Show

instance Functor Tree where
   fmap f Empty = Empty
   fmap f (Node a xs) = Node (f a) (fmap (fmap f) xs)

singleton :: a -> Tree a
singleton x = Node x []

treeInsert :: a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node y ys) = Node y (singleton x:ys)
