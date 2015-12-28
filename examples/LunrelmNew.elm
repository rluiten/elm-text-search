module Examples.LunrelmNew
    ( codeForLunrelmNewExample
    , ExampleDocType
    , createNewIndexExample
    , updatedMyIndexAfterAdd
    ) where

{-| Create an index and add a document.

@docs codeForLunrelmNewExample
@docs ExampleDocType
@docs createNewIndexExample
@docs updatedMyIndexAfterAdd

Copyright (c) 2016 Robin Luiten
-}

import Lunrelm


{-| Code for this example.

```
import Lunrelm

type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


createNewIndexExample : Lunrelm.Index ExampleDocType
createNewIndexExample =
  Lunrelm.new
    { ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    }

{-| Add a document to an index. -}
updatedMyIndexAfterAdd : Result String (Lunrelm.Index ExampleDocType)
updatedMyIndexAfterAdd =
    Lunrelm.add
      createNewIndexExample
      { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this document."
      }
```

-}
codeForLunrelmNewExample : String
codeForLunrelmNewExample = "Place holder variable to add documentation."


{-| Example document type. -}
type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


{-| Create a Lunrelm index with default configuration.

The provided configuration has the following meaning
* ref
 * The unique document reference will be extracted from each
   document using the `.cid` function.
* fields
 * The following fields will be indexed from each document
  * `.title`
  * `.body`
 * When searching the index any word matches found in the
   `.title` field (boost value 5) raise the document match score
   more than if found in the `.body` field (boost value 1).
  * The document match score determines the order of the list
    of matching documents returned.


-}
createNewIndexExample : Lunrelm.Index ExampleDocType
createNewIndexExample =
  Lunrelm.new
    { ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    }


{-| Add a document to an index. -}
updatedMyIndexAfterAdd : Result String (Lunrelm.Index ExampleDocType)
updatedMyIndexAfterAdd =
    Lunrelm.add
      createNewIndexExample
      { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this document."
      }
