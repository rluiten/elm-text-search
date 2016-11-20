module Utils exposing
  ( intersectSets
  )

{-| Some misc utils

@docs intersectSets

Copyright (c) 2016 Robin Luiten
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
