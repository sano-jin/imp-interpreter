module Util exposing (..)

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

curry : ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry3 : (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

curry3 : ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

