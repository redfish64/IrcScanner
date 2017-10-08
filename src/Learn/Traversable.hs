module Traversable where

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Foldable Tree where
  foldr _ s Empty = s
  foldr f s (Leaf x) = f x s
  foldr f s (Node l k r) = foldr f (f k (foldr f s l)) r

  
instance Traversable Tree where
    traverse _ Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l k r) = (Node <$> traverse f l) <*> f k <*> traverse f r

    
half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

rep :: Int -> [Int]
rep x = replicate x x
