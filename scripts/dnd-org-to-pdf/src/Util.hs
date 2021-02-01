module Util where

infixl 0 |>
(|>) :: a -> (a -> b) -> b
arg |> fun = fun arg

