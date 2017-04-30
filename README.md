# ElmTextSearch full text indexer

Copyright (c) 2016-2017 Robin Luiten

This is a full text indexing engine inspired by lunr.js and written in Elm language.
See http://lunrjs.com/ for lunr.js

While ElmTextSearch has a good selection of tests this library is not battle tested and may contain some performance issues.

I am happy to hear about users of this package.

I am happy to receive contributions be they bug reports, pull requests, documention updates or examples.

### v4.0.0 will not load indexes saved with old version.

If you do not use `storeToValue` `storeToString` `fromString` `fromValue` in ElmTextSearch this update is not likely to introduce issues.

The way that filters and transforms are applied to the content of documents has changed.
This is to properly fix a bug reported see https://github.com/rluiten/elm-text-search/issues/10 where stop word filters werer not correctly applied. This means saved indexes from prevoius version of ElmTextSearch will not load in this version.

* `Defaults.indexVersion` has changed value.

The reason this is a Major version bump is some generalisation was done to enable future support
for loading and saving of older version and types of default index confgurations.


### Packages

Several packages were created for this project and published seperately for this package to depend on.

* trie
 * http://package.elm-lang.org/packages/rluiten/trie/latest
* stemmer
 * http://package.elm-lang.org/packages/rluiten/stemmer/latest
* sparsevector
 * http://package.elm-lang.org/packages/rluiten/sparsevector/latest

### Parts of lunr.js were left out

 * This does not have an event system.
 * Its internal data structure is not compatible.

### Notes captured along way writing this.

* lunr.js
 * tokenStore.remove does not decrement length, but it doesnt use length really only save/load
 * stemmer "lay" -> "lay" "try" -> "tri" is opposite to porter stemmer
* porter stemmer erlang implementation
 * step5b does not use endsWithDoubleCons which is required afaik to pass the voc.txt output.txt cases


### Example

See examples folder for three examples.
First example is included inline here.

IndexNewAddSearch.elm
```elm
import ElmTextSearch
import Html exposing (Html, div, text)


{-| Example document type. -}
type alias ExampleDocType =
  { cid : String
  , title : String
  , author : String
  , body : String
  }


{-| Create an index with default configuration.
See ElmTextSearch.SimpleConfig documentation for parameter information.
-}
createNewIndexExample : ElmTextSearch.Index ExampleDocType
createNewIndexExample =
  ElmTextSearch.new
    { ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    , listFields = []
    }


{-| Add a document to an index. -}
resultUpdatedMyIndexAfterAdd :
  Result String (ElmTextSearch.Index ExampleDocType)
resultUpdatedMyIndexAfterAdd =
  ElmTextSearch.add
    { cid = "id1"
    , title = "First Title"
    , author = "Some Author"
    , body = "Words in this example document with explanations."
    }
    createNewIndexExample


{-| Search the index.

The result includes an updated Index because a search causes internal
caches to be updated to improve overall performance.
-}
resultSearchIndex :
  Result String
    ( ElmTextSearch.Index ExampleDocType
    , List (String, Float)
    )
resultSearchIndex =
  resultUpdatedMyIndexAfterAdd
    |> Result.andThen
      (ElmTextSearch.search "explanations")


{-| Display search result. -}
main =
  let
    -- want only the search results not the returned index
    searchResults = Result.map Tuple.second resultSearchIndex
  in
    div []
    [ text
        (
          "Result of searching for \"explanations\" is "
            ++ (toString searchResults)
        )
    ]
```
