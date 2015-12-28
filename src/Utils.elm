module Utils
    ( intersectSets
    , apply
    , (|:)
    ) where

{-| Some misc utils

@docs intersectSets
@docs apply
@docs |:

Copyright (c) 2016 Robin Luiten
-}

import Json.Decode exposing (..)
import Set exposing (Set)

{-| Return intersection of a list of sets
-}
intersectSets : List (Set String) -> Set String
intersectSets sets =
    case sets of
      [] -> Set.empty
      h :: tail ->
        List.foldr (\set agg -> Set.intersect set agg) h tail


{-| Utility to improve decoding records.

`apply` and `|:` pulled from
https://github.com/circuithub/elm-json-extra/blob/master/src/Json/Decode/Extra.elm#L90-L113

See https://github.com/NoRedInk/elm-rails/wiki/NoRedInk's-Elm-Style-Guide
-}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
    f `andThen` (\f' -> f' `map` aDecoder)


{-| See `apply` docs. -}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    apply
