module TokenProcessors exposing
    ( tokenizer
    , tokenizerWith
    , tokenizerWithRegex
    , trimmer
    )

{-| TokenProcessors for strings.

## Create a tokenizer

@docs tokenizer
@docs tokenizerWith
@docs tokenizerWithRegex

## Word transformer

@docs trimmer

Copyright (c) 2016 Robin Luiten
-}

import Regex exposing
    ( Regex
    , HowMany(All)
    , regex
    , split
    , replace)
import String exposing ( trim, toLower )


defaultSeparator : Regex
defaultSeparator = regex "[\\s\\-]+"


{-| Tokenize a string.

Will not return any empty string tokens.

By default this splits on whitespace and hyphens.
-}
tokenizer : String -> List String
tokenizer = tokenizerWithRegex defaultSeparator


{-| Tokenize a string.

Will not return any empty string tokens.

Supply your own regex for splitting the string.
-}
tokenizerWithRegex : Regex -> String -> List String
tokenizerWithRegex seperatorRegex data =
    let
      splitter = (split All seperatorRegex) << toLower << trim
    in
    List.filter
      (\token -> (String.length token) > 0)
      (splitter data)


{-| Tokenize a string.

Will not return any empty string tokens.

Supply your own String which is turned into a regex for splitting the string.
-}
tokenizerWith : String -> String -> List String
tokenizerWith seperatorPattern =
    tokenizerWithRegex (regex seperatorPattern)


-- not sure want to do this here maybe it belongs elsewhere
-- deling with List (Maybe String) or something
-- tokenizerArray : List String -> List String
-- tokenizerArray = List.map toLower


{-| Remove non word characters from start end of tokens
-}
trimmer : String -> String
trimmer =
    replace All (regex "^\\W+|\\W+$") (\_ -> "")
