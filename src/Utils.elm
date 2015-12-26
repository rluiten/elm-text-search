module Utils
    ( intersectSets
    ) where

{-| General utils

Copyright (c) 2016 Robin Luiten

@docs intersectSets

-}

import Set exposing (Set)

{-| Return intersection of a list of sets
-}
intersectSets : List (Set String) -> Set String
intersectSets sets =
    case sets of
      [] -> Set.empty
      h :: tail ->
        List.foldr (\set agg -> Set.intersect set agg) h tail
