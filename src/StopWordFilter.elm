module StopWordFilter exposing
    ( createDefaultFilterFunc
    , stopEnglishWordList
    , createFilterFuncWith
    , createFilterFunc
    )

{-| StopWordFilter is an English language stop word list filter, any words
contained in the list are not stored in the index.

This is intended to be used in the ElmTextSearch token processing pipeline.


### Things to know about stop word lists.

  - Words in document are split on white space to create tokens.
  - Tokens have non word characters from prefix and suffix to improve matching filters.
  - Input tokens to create stop word filters should be full words.
  - It is more efficient to merge all your stop words into a single
    stop word filter.


## create default stop word filter func

@docs createDefaultFilterFunc


## A default stop word english filter list

@docs stopEnglishWordList


## Create a custom stop word filter list

@docs createFilterFuncWith
@docs createFilterFunc

Copyright (c) 2016 Robin Luiten

-}

import Index.Model exposing (FilterFactory)
import Set


{-| Default english stop word list to create filter.
-}
stopEnglishWordList : List String
stopEnglishWordList =
    [ "a"
    , "able"
    , "about"
    , "across"
    , "after"
    , "all"
    , "almost"
    , "also"
    , "am"
    , "among"
    , "an"
    , "and"
    , "any"
    , "are"
    , "as"
    , "at"
    , "be"
    , "because"
    , "been"
    , "but"
    , "by"
    , "can"
    , "cannot"
    , "could"
    , "dear"
    , "did"
    , "do"
    , "does"
    , "either"
    , "else"
    , "ever"
    , "every"
    , "for"
    , "from"
    , "get"
    , "got"
    , "had"
    , "has"
    , "have"
    , "he"
    , "her"
    , "hers"
    , "him"
    , "his"
    , "how"
    , "however"
    , "i"
    , "if"
    , "in"
    , "into"
    , "is"
    , "it"
    , "its"
    , "just"
    , "least"
    , "let"
    , "like"
    , "likely"
    , "may"
    , "me"
    , "might"
    , "most"
    , "must"
    , "my"
    , "neither"
    , "no"
    , "nor"
    , "not"
    , "of"
    , "off"
    , "often"
    , "on"
    , "only"
    , "or"
    , "other"
    , "our"
    , "own"
    , "rather"
    , "said"
    , "say"
    , "says"
    , "she"
    , "should"
    , "since"
    , "so"
    , "some"
    , "than"
    , "that"
    , "the"
    , "their"
    , "them"
    , "then"
    , "there"
    , "these"
    , "they"
    , "this"
    , "tis"
    , "to"
    , "too"
    , "twas"
    , "us"
    , "wants"
    , "was"
    , "we"
    , "were"
    , "what"
    , "when"
    , "where"
    , "which"
    , "while"
    , "who"
    , "whom"
    , "why"
    , "will"
    , "with"
    , "would"
    , "yet"
    , "you"
    , "your"
    ]


{-| Default english stop word filter suitable for ElmTextSearch.
-}
createDefaultFilterFunc : FilterFactory doc
createDefaultFilterFunc index =
    createFilterFunc stopEnglishWordList index


{-| Create stop word list filter suitable for ElmTextSearch, this versions
extends the default word list with the extra words provided.
-}
createFilterFuncWith : List String -> FilterFactory doc
createFilterFuncWith extraWords index =
    createFilterFunc (List.append extraWords stopEnglishWordList) index


{-| Create stop word filter for provided list of tokens suitable for ElmTextSearch.

\*\* This creates a stop world filter purely from your own word list, understand
what you are doing and consequences if you use this. \*\*

The FilterFunc created returns True to allow words into index.
So words found in the stopWordList return False

-}
createFilterFunc : List String -> FilterFactory doc
createFilterFunc tokens index =
    let
        tokenSet =
            Set.fromList tokens
    in
    ( index, \word -> not (Set.member word tokenSet) )
