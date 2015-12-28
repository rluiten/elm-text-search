module Examples.LunrelmNewWith
    ( codeForLunrelmNewWithExample
    , ExampleDocType
    , createNewWithIndexExample
    , addDocToIndexExample
    , searchIndexExample
    ) where

{-| Create an index with customized
stop word filter using Lunrelm.newWith

@docs codeForLunrelmNewWithExample
@docs ExampleDocType
@docs createNewWithIndexExample
@docs addDocToIndexExample
@docs searchIndexExample

Copyright (c) 2016 Robin Luiten
-}

import Lunrelm
import IndexDefaults
import StopWordFilter


{-| Code for this example.
```

import Lunrelm
import IndexDefaults
import StopWordFilter


type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


createMyStopWordFilter =
    StopWordFilter.createFilterFuncWith
      [ "electronic" ]


createNewWithIndexExample : Lunrelm.Index ExampleDocType
createNewWithIndexExample =
  Lunrelm.newWith
    { indexType = "Lunrelm - For paw paw automation index v1"
    , ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    , transformFactories = IndexDefaults.defaultTransformFactories
    , filterFactories = [ createMyStopWordFilter ]
    }


addDocToIndexExample : Result String (Lunrelm.Index ExampleDocType)
addDocToIndexExample =
    Lunrelm.add
      createNewWithIndexExample
      { cid = "id1"
      , title = "The Pawpaw Harvester Model G45"
      , author = "Pawpaw Fan"
      , body = "It can help you harvest pawpaws. It also does avocado."
      }


searchIndexExample :
    Result
      String
      ( Result
          String
          ( Lunrelm.Index ExampleDocType, List (String, Float) )
      )
searchIndexExample =
    Result.map
      (\index ->
        Lunrelm.search
          index
          "avocado"
      )
      addDocToIndexExample


```
-}
codeForLunrelmNewWithExample : String
codeForLunrelmNewWithExample = "Place holder variable to add documentation."


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

It is possible to completely replace the stop word list.
-}
createMyStopWordFilter =
    StopWordFilter.createFilterFuncWith
      [ "electronic" ]


{-| Create a Lunrelm index with extra options.

In this case a customized stop word filter is provided.

This example is replacing the stop word filter only.

It is still supplying the default transform factories.

Supply an index type for your customized index config. This
becomes important when loading back saved index.

It is a good idea to include a version in your index type string
in case you update things and might still have old versions
around that you need to work with.

See Examples.LunrelmNew for informatoin about the `ref` and `fields` fields.
-}
createNewWithIndexExample : Lunrelm.Index ExampleDocType
createNewWithIndexExample =
  Lunrelm.newWith
    { indexType = "Lunrelm - For paw paw automation index v1"
    , ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    , transformFactories = IndexDefaults.defaultTransformFactories
    , filterFactories = [ createMyStopWordFilter ]
    }


{-| Adding a document to the index. -}
addDocToIndexExample : Result String (Lunrelm.Index ExampleDocType)
addDocToIndexExample =
    Lunrelm.add
      createNewWithIndexExample
      { cid = "id1"
      , title = "The Pawpaw Harvester Model G45"
      , author = "Pawpaw Fan"
      , body = "It can help you harvest pawpaws. It also does avocado."
      }


{-| Search the index.

The result includes an updated Index because a search causes internal
caches to be updated to improve overall performance.
-}
searchIndexExample :
    Result
      String
      ( Result
        String
        ( Lunrelm.Index ExampleDocType, List (String, Float) )
      )
searchIndexExample =
    Result.map
      (\index ->
        Lunrelm.search
          index
          "avocado"
      )
      addDocToIndexExample
