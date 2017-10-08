module Applicative where
import Data.Monoid((<>))

data E a b = L a | R b deriving (Show)

instance Functor (E x) where
  fmap _ (L a) = L a
  fmap f (R r) = R (f r)

instance Monoid e => Applicative (E e) where
  pure = R
  R f <*> R a = R (f a)     -- neutral
  L  e <*> R _ = L e          -- short-circuit
  R _ <*> L  e = L e          -- short-circuit
  L e1 <*> L e2 = L (e1 <> e2) -- combine!

x :: E [Integer] Integer
x = R (+ 1) <*> (L [1] <*> L [2])

x' :: E [Integer] (Integer -> Integer)
x' = R (+ 1)

ifA' :: Applicative f => f Bool -> f a -> f a -> f a
ifA' b t e = (pure (\b' t' e' -> if b' then t' else e')) <*> b <*> t <*> e
