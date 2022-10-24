module Stark.Prelude
  ( uncurry4, (<$$>),
  )
where

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x
