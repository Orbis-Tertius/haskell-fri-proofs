module Stark.Prelude
  ( uncurry4,
    uncurry5,
    (<$$>),
  )
where

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x
