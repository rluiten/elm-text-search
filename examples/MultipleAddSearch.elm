module Main exposing (..)

{-| Create an index and add multiple documents.

Copyright (c) 2016-2017 Robin Luiten

-}

import ElmTextSearch
import Html exposing (Html, div, text)


{-| Example document type.
-}
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


documents =
    [ { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this example document with explanations."
      }
    , { cid = "id2"
      , title = "Is a cactus as pretty as a tree ?"
      , author = "Joe Greeny"
      , body = "This title contains information about cactuses."
      }
    ]


{-| Add a documents to index.

If any add result is an Err this returns the first failure.

-}
indexWithMulitpleDocumentsAdded : ( ElmTextSearch.Index ExampleDocType, List ( Int, String ) )
indexWithMulitpleDocumentsAdded =
    ElmTextSearch.addDocs
        documents
        createNewIndexExample


{-| Search the index.

The result includes an updated Index because a search causes internal
caches to be updated to improve overall performance.

This is ignoring any errors from call to addAllDocs
in indexWithMulitpleDocumentsAdded.

-}
resultSearchIndex : Result String ( ElmTextSearch.Index ExampleDocType, List ( String, Float ) )
resultSearchIndex =
    ElmTextSearch.search "title" (Tuple.first indexWithMulitpleDocumentsAdded)


{-| Display search result.
-}
main =
    let
        -- want only the search results not the returned index
        searchResults =
            Result.map Tuple.second resultSearchIndex
    in
        div []
            [ text
                ("Result of searching for \"explanations\" is "
                    ++ (toString searchResults)
                )
            ]
