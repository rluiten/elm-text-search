module LunrelmNew
    ( ExampleDocType
    , myIndex
    , updatedMyIndexAfterAdd) where

{-| Create an index and add a document.

@docs ExampleDocType
@docs myIndex
@docs updatedMyIndexAfterAdd

-}

import Lunrelm


{-| Example document type -}
type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


{-| Lunrelm index with default configuration.

Data from the title and body fields from the document
will be indexed. When searching words that match a title
are a better match than the body due to the 5 boost on title.
-}
myIndex =
  Lunrelm.new
    { ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    }

updatedMyIndexAfterAdd =
    Lunrelm.add
      myIndex
      { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this document."
      }
