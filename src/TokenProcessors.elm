module TokenProcessors exposing
    ( tokenizer
    , tokenizerList
    , tokenizerWith
    , tokenizerWithRegex
    , tokenizerWithRegexList
    , trimmer
    , tokenizerWithList
    )

{-| TokenProcessors for strings.


## Create a tokenizer

@docs tokenizer
@docs tokenizerList
@docs tokenizerWith
@docs tokenizerWithRegex
@docs tokenizerWithRegexList


## Word transformer

@docs trimmer

Copyright (c) 2016 Robin Luiten

-}

import Regex
    exposing
        ( Regex
          -- , HowMany(..)
        , fromString
        , replace
        , split
        )
import String exposing (toLower, trim)


forceRegex : String -> Regex
forceRegex =
    Maybe.withDefault Regex.never << fromString


defaultSeparator : Regex
defaultSeparator =
    forceRegex "[\\s\\-]+"


{-| Tokenize a String.
Will not return any empty string tokens.
By default this splits on whitespace and hyphens.
-}
tokenizer : String -> List String
tokenizer =
    tokenizerWithRegex defaultSeparator


{-| Tokenize a List String.
Will not return any empty string tokens.
By default this splits on whitespace and hyphens.
-}
tokenizerList : List String -> List String
tokenizerList =
    tokenizerWithRegexList defaultSeparator


{-| Tokenize a string.
Will not return any empty string tokens.
Supply your own regex for splitting the string.
-}
tokenizerWithRegex : Regex -> String -> List String
tokenizerWithRegex seperatorRegex data =
    let
        splitter =
            split seperatorRegex << toLower << trim
    in
    List.filter
        (\token -> String.length token > 0)
        (splitter data)


tokenizerWithRegexList : Regex -> List String -> List String
tokenizerWithRegexList seperatorRegex listData =
    let
        splitter =
            split seperatorRegex << toLower << trim

        -- List.foldr (\set agg -> Set.intersect set agg) h tail
        -- tokens : List String
        tokens =
            List.foldr
                (\str agg ->
                    List.append agg (splitter str)
                )
                []
                listData
    in
    List.filter
        (\token -> String.length token > 0)
        tokens


{-| Tokenize a String.
Will not return any empty string tokens.
Supply your own String which is turned into a regex for splitting the string.
-}
tokenizerWith : String -> String -> List String
tokenizerWith seperatorPattern =
    tokenizerWithRegex (forceRegex seperatorPattern)


{-| Tokenize a List String.
Will not return any empty string tokens.
Supply your own String which is turned into a regex for splitting the string.
-}
tokenizerWithList : String -> List String -> List String
tokenizerWithList seperatorPattern =
    tokenizerWithRegexList (forceRegex seperatorPattern)


trimmerRegex =
    forceRegex "^\\W+|\\W+$"


{-| Remove non word characters from start and end of tokens
-}
trimmer : String -> String
trimmer =
    replace trimmerRegex (\_ -> "")
