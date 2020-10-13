module Util exposing (..)

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

curry : ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

