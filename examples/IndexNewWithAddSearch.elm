{-| Create an index with customized stop word filter using
ElmTextSearch.newWith.

Copyright (c) 2016 Robin Luiten
-}

import ElmTextSearch
import Graphics.Element exposing (show)
import IndexDefaults
import StopWordFilter


{-| Example document type. -}
type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


{-| Create an extended stop word filter.

Be careful about adding words to your stop word list, as any stop word
will not be indexed and you will not be able to search for the word in
documents as it will not be found.

It is possible to completely replace the stop word list and not
just extend it.
-}
createMyStopWordFilter =
    StopWordFilter.createFilterFuncWith
      [ "explanations" ]


{-| Create an index with extra options.

* In this case a customized stop word filter is provided.
* It is supplying the default transform factories.
* It supplies an index type for the customized index config.
This becomes important when loading back saved index.
* It is a good idea to include a version in your index type string
in case you update things and might still have old versions
around that you need to work with.
-}
createNewWithIndexExample : ElmTextSearch.Index ExampleDocType
createNewWithIndexExample =
  ElmTextSearch.newWith
    { indexType = "ElmTextSearch - Customized Stop Words v1"
    , ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    , transformFactories = IndexDefaults.defaultTransformFactories
    , filterFactories = [ createMyStopWordFilter ]
    }


{-| Adding a document to the index. -}
addDocToIndexExample :
      Result String (ElmTextSearch.Index ExampleDocType)
addDocToIndexExample =
    ElmTextSearch.add
      { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this example document with explanations."
      }
      createNewWithIndexExample


{-| Search the index for a word in our extended stop words.
This will return no matches.
-}
firstResultSearchIndex :
    Result String
      ( ElmTextSearch.Index ExampleDocType
      , List (String, Float)
      )
firstResultSearchIndex =
    addDocToIndexExample
      `Result.andThen`
      (ElmTextSearch.search "explanation")


{-| Search the index for a word that is not a stop word.
It will return an Err about no search terms.
-}
secondResultSearchIndex :
    Result String
      ( ElmTextSearch.Index ExampleDocType
      , List (String, Float)
      )
secondResultSearchIndex =
    addDocToIndexExample
      `Result.andThen`
      (ElmTextSearch.search "examples")


{-| Display search result. -}
main =
    let
      searchResults1 =
        Result.map snd firstResultSearchIndex
      searchResults2 =
        Result.map snd secondResultSearchIndex
    in
      show
        [ "Result of first search for \"explanation\" is "
           ++ (toString searchResults1)
        , "Result of second search for \"examples\" is "
           ++ (toString searchResults2)
        ]
