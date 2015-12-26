module LunrelmNewWith
    ( ExampleDocType
    , myIndex
    , updatedMyIndexAfterAdd) where

{-| Create an index with customized
stop word filter using Lunrelm.newWith

@docs ExampleDocType
@docs myIndex
@docs updatedMyIndexAfterAdd

-}

import Lunrelm
import IndexUtils
import TokenProcessors
import Stemmer
import StopWordFilter

{-| Example document type -}
type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


myStopWords = ["electronic", "harvesting", "pawpaw"]
createMyStopWordFilter =
  StopWordFilter.createFilterFuncWith myStopWords


{-| Lunrelm index with customized stop word filter.

This example is replacing the stop word filter only
it is supplying the default transform factories.

Supply an index type for your customized index config. This
becomes important when loading back saved index.

It is a good idea to include a version in your index type string
in case you update things and might still have old versions
around that you need to work with.

-}
myIndex =
  Lunrelm.newWith
    { indexType = "Lunrelm - For paw paw automation index v1"
    , ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    , transformFactories =
        [ IndexUtils.createFuncCreator TokenProcessors.trimmer
        , IndexUtils.createFuncCreator Stemmer.stem
        ]
    , filterFactories =
        [ createMyStopWordFilter
        ]
    }


updatedMyIndexAfterAdd =
    Lunrelm.add
      myIndex
      { cid = "id1"
      , title = "The Pawpaw Harvestor Model G45"
      , author = "Pawpaw Fan"
      , body = "We harvest help you harvest pawpaws."
      }
