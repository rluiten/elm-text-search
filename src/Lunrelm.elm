module Lunrelm
    ( Index
    , LunrelmSimpleConfig
    , Config
    , new
    , newWith
    , add
    , remove
    , update
    , search
    , storeToValue
    , storeToString
    , fromString
    , fromValue
    , fromStringWith
    , fromValueWith
    ) where

{-| Lunrelm a full text indexer written in Elm language inspired by lunr.js.

A useful article about lunr.js
https://www.new-bamboo.co.uk/blog/2013/02/26/full-text-search-in-your-browser/

## Create Index
@docs new
@docs newWith

## Modify Index
@docs add
@docs remove
@docs update

## Query Index
@docs search

## Types
@docs Index
@docs Config
@docs LunrelmSimpleConfig

## Save and Load a Lunerlm Index

* You can save an index using [`Lunrelm.Json.Encoder.encoder`](Lunrelm.Json.Encoder#encoder)
* You can load a saved index using
  [`Lunrelm.Json.Decoder.decoder`](Lunrelm.Json.Decoder#decoder)
  to produce a `CodecIndexRecord`.
* You can save a CodecIndexRecord using [`Lunrelm.Json.Encoder.encoder`](Lunrelm.Json.Encoder#encoder)
* ** Modifying an index outside of Lunrelm using the Decoder and Encoder directly
may cause it to not work correctly loaded into Lurelm. **

@docs storeToValue
@docs storeToString
@docs fromString
@docs fromValue
@docs fromStringWith
@docs fromValueWith

Copyright (c) 2016 Robin Luiten
-}

import Json.Decode as Decode
import Json.Encode as Encode

import Index
import IndexDefaults
import IndexLoad
import IndexModel
import Lunrelm.Json.Encoder as LunrelmEncoder

import IndexUtils
import Stemmer
import StopWordFilter
import TokenProcessors


{-| Lunrelm Index
-}
type alias Index doc = Index.Index doc


{-| Lunrelm SimpleConfig. -}
type alias LunrelmSimpleConfig doc =
    { ref : (doc -> String)
    , fields : List (doc -> String, Float)
    }


{-| Lunrelm configuration. -}
type alias Config doc = IndexModel.Config doc


{- convert Lunrelm.LunrelmSimpleConfig to IndexModel.SimpleConfig -}
getIndexSimpleConfig : LunrelmSimpleConfig doc -> IndexModel.SimpleConfig doc
getIndexSimpleConfig {ref, fields} =
    { indexType = IndexDefaults.lunrelmIndexType
    , ref = ref
    , fields = fields
    }


{-| Create new Lunrelm index.

Example

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
```

### The default Lunrelm transform factories.

```
    IndexDefaults.defaultTransformFactories
```


### The default Lunrelm filter factories.

```
    IndexDefaults.defaultFilterFactories
```

-}
new : LunrelmSimpleConfig doc -> Index doc
new simpleConfig =
    Index.new (getIndexSimpleConfig simpleConfig)


{-| Create new Lunrelm index with additional configuration.

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
```

-}
newWith : Config doc -> Index doc
newWith = Index.newWith


{-| Add a document to Lunrelm index.

Starting with the Lunrelm.new example above this adds a document.
```
    updatedIndex =
      Lunrelm.add
        indexMyDocs
        { cid = "123"
        , title = "Examples of a Banana"
        , author = "Sally Apples"
        , body = "Sally writes words about a banana."
        }
```

Conditions that cause a result Err with message.
* Error document ref is empty.
* Error after tokenisation there are no terms to index.
* Error adding document that allready exists.
-}
add : Index doc -> doc -> Result String (Index doc)
add = Index.add

{-| Remove a document from Lunrelm index.

Starting with the Lunrelm.new example above this removes a document.
```
    updatedIndex =
      Lunrelm.remove
        createNewIndexExample
        { cid = "123"
        , title = "Examples of a Banana"
        , author = "Sally Apples"
        , body = "Sally writes words about a banana."
        }
```

Conditions that cause a result Err with message.
* Error document has an empty unique id (ref).
* Error document is not in index.
-}
remove : Index doc -> doc -> Result String (Index doc)
remove = Index.remove


{-| Update a doc in Lunrelm index.

Starting with the Lunrelm.new example above this updates a document.
```
    updatedIndex =
      Lunrelm.remove
        createNewIndexExample
        { cid = "123"
        , title = "Examples of a Bananas in every day life."
        , author = "Sally Apples"
        , body = "Sally writes more words about a banana."
        }
```

Conditions that cause an error result are those for
[`Lunrelm.remove`](Lunrelm#remove) and then
[`Lunrelm.add`](Lunrelm#add).

-}
update : Index doc -> doc -> Result String (Index doc)
update = Index.update


{-| Search Lunrelm index with query.


Queries are a string.

Tokens are extracted from the query string and passed through the
same processing used when indexing documents.

Each token is expanded, so that the term "he" might be expanded to "hello"
and "help" if those terms were already included in the document index.

Multiple tokens are allowed and will lead to an AND based query.

The following example runs a search for documents containing both "apple" and "banana".

```
    (updatedMyIndex, results) =
      Index.search createNewIndexExample "Apple banana"
```

Results are a list of matching document reference identifiers with
there similarity to query score, ordered by score ascending.

A possibly updated index is returned from search as well `updatedMyIndex`.
This is because the data model is updated with searches to cache information
to improve overall perfromance, it is expected after sufficient searches the
index will no longer update due to searches.

Adding or removing a new document will cause some of the internal caching to be reset.

Conditions that cause a result Err with message.
* Error there are no documents in index to search.
* Error query is empty.
* Error after tokenisation there are no terms to search for.

-}
search : Index doc -> String -> Result String (Index doc, List (String, Float))
search = Index.search


{-| Store a Lunrelm index to a Value.

You can also use [`Lunrelm.Json.Encoder`](Lunrelm.Json.Encoder).
-}
storeToValue : Index doc -> Encode.Value
storeToValue =
    LunrelmEncoder.encoder


{-| Store a Lunrelm index to a String.

You can also use [`Lunrelm.Json.Encoder`](Lunrelm.Json.Encoder).
-}
storeToString : Index doc -> String
storeToString index =
    Encode.encode 0 (LunrelmEncoder.encoder index)


{-| Create an Index from a String which has a stored Index in it and the
supplied basic configurations.

See [`Lunrelm.fromStringWith`](Lunrelm#fromStringWith) for possible Err results.
-}
fromString : LunrelmSimpleConfig doc -> String -> Result String (Index doc)
fromString simpleConfig inputString =
    IndexLoad.loadIndex
      (getIndexSimpleConfig simpleConfig)
      inputString


{-| Create an Index from a Value which has a stored Index in it.

See [`Lunrelm.fromStringWith`](Lunrelm#fromStringWith) for possible Err results.
-}
fromValue : LunrelmSimpleConfig doc -> Decode.Value -> Result String (Index doc)
fromValue simpleConfigs inputString =
    Err "load is not implemented"


{-| Create an Index from a String which has a stored Index in it.

If none of the indexVersion in the list of Config match the index
type being loaded it will return an Err.

The list of configurations wil be searched for a matching indexType
so you should provide configs for all types you may be trying to load.
No more than the config that matches is required though.

If the none of the supplied Config match the loaded Index then it
will try if the index being loaded matches the default version if so
it will still load the index.

The following Err results may be returned.
* "Error cannot load Lunrelm Index. Tried to load index of type \"__IndexTest Type -\". It is not in supported index configurations."
 * It contains the loaded version index type which comes from input.
* "Error cannot load Lunrelm Index. Version supported is 1.0.0. Version tried to load is 1.0.1."
 * It includes both expected and loaded versions which may vary.

-}
fromStringWith : List (Config doc) -> String -> Result String (Index doc)
fromStringWith =
    IndexLoad.loadIndexWith


{-| Create an Index from a String which has a stored Index in it.

If none of the indexVersion in the list of SimpleConfig match the index
being decoded it will return an Err.

See [`Lunrelm.fromStringWith`](Lunrelm#fromStringWith) for possible Err results.
-}
fromValueWith : List (Config doc) -> Decode.Value -> Result String (Index doc)
fromValueWith =
    IndexLoad.loadIndexValueWith
