module ElmTextSearch
    ( Index
    , SimpleConfig
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

{-| A full text indexer written in Elm language inspired by lunr.js.

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
@docs SimpleConfig

## Save and Load an Index

* You can save an index using [`ElmTextSearch.Json.Encoder.encoder`](ElmTextSearch.Json.Encoder#encoder)
* You can load a saved index using
  [`ElmTextSearch.Json.Decoder.decoder`](ElmTextSearch.Json.Decoder#decoder)
  to produce a [`IndexModel.CodecIndexRecord`](IndexModel#CodecIndexRecord).
* You can save a [`IndexModel.CodecIndexRecord`](IndexModel#CodecIndexRecord)
  using [`ElmTextSearch.Json.Encoder.codecIndexRecordEncoder`](ElmTextSearch.Json.Encoder#codecIndexRecordEncoder)
* ** Modifying an index outside of ElmTextSearch using the Decoder and Encoder directly
may cause it to not work correctly loaded into ElmTextSearch. **

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
import ElmTextSearch.Json.Encoder as IndexEncoder

import IndexUtils
import Stemmer
import StopWordFilter
import TokenProcessors


{-| An Index holds the data to be able search for added documents.
-}
type alias Index doc = Index.Index doc


{-| A SimpleConfig is the least amount of configuration data
required to create an Index.
-}
type alias SimpleConfig doc =
    { ref : (doc -> String)
    , fields : List (doc -> String, Float)
    }


{-| A Config is required to create an Index. -}
type alias Config doc = IndexModel.Config doc


{- convert ElmTextSearch.SimpleConfig to IndexModel.SimpleConfig
-}
getIndexSimpleConfig : SimpleConfig doc -> IndexModel.SimpleConfig doc
getIndexSimpleConfig {ref, fields} =
    { indexType = IndexDefaults.elmTextSearchIndexType
    , ref = ref
    , fields = fields
    }


{-| Create new index.

Example
```
import ElmTextSearch

type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


createNewIndexExample : ElmTextSearch.Index ExampleDocType
createNewIndexExample =
  ElmTextSearch.new
    { ref = .cid
    , fields =
        [ ( .title, 5.0 )
        , ( .body, 1.0 )
        ]
    }
```

The `SimpleConfig` parameter to new is
* ref
 * The unique document reference will be extracted from each
   document using `.cid`.
* fields
 * The following fields will be indexed from each document
  * `.title`
  * `.body`
 * When searching the index any word matches found in the
   `.title` field (boost value 5.0) raise the document match score
   more than if found in the `.body` field (boost value 1.0).
  * The document match score determines the order of the list
    of matching documents returned.
-}
new : SimpleConfig doc -> Index doc
new simpleConfig =
    Index.new (getIndexSimpleConfig simpleConfig)


{-| Create new index with additional configuration.

Example.
```
import ElmTextSearch
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
      [ "explanations" ]


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
```
-}
newWith : Config doc -> Index doc
newWith = Index.newWith


{-| Add a document to an index.

Starting with the ElmTextSearch.new example above this adds a document.
```
addDocToIndexExample =
    ElmTextSearch.add
      { cid = "id1"
      , title = "First Title"
      , author = "Some Author"
      , body = "Words in this example document with explanations."
      }
      createNewWithIndexExample
```

Conditions that cause a result Err with message.
* Error document ref is empty.
* Error after tokenisation there are no terms to index.
* Error adding document that allready exists.
-}
add : doc -> Index doc -> Result String (Index doc)
add =
    Index.add


{-| Remove a document from an index.

Starting with the ElmTextSearch.new example above this removes a document.
```
removeDocFromIndexExample =
    ElmTextSearch.remove
      { cid = "123"
      , title = "Examples of a Banana"
      , author = "Sally Apples"
      , body = "Sally writes words about a banana."
      }
      createNewIndexExample
```

Conditions that cause a result Err with message.
* Error document has an empty unique id (ref).
* Error document is not in index.
-}
remove : doc -> Index doc -> Result String (Index doc)
remove =
    Index.remove


{-| Update a document in an index.

Starting with the ElmTextSearch.new example above this updates a document.
```
    updatedIndex =
      ElmTextSearch.update
        { cid = "123"
        , title = "Examples of a Bananas in every day life."
        , author = "Sally Apples"
        , body = "Sally writes more words about a banana."
        }
        createNewIndexExample
```

Conditions that cause an error result are those for
[`ElmTextSearch.remove`](ElmTextSearch#remove) and
[`ElmTextSearch.add`](ElmTextSearch#add).
-}
update : doc -> Index doc -> Result String (Index doc)
update =
    Index.update


{-| Search an index with query.

Tokens are extracted from the query string and passed through the
same processing used when indexing documents.

Each token is expanded, so that the term "he" might be expanded to "hello"
and "help" if those terms were already included in the document index.

Multiple tokens are allowed and will lead to an AND based query.

The following example runs a search for documents containing both "apple" and "banana".

```
searchResult =
    Index.search "Apple banana" createNewIndexExample
```

Results are a list of matching document reference identifiers with
there similarity to query score, ordered by score descending, so the
best matches are earliest in the list.

An index is returned from search as well. This is because the data model may
be updated to improve performance for later searches.

Adding or removing a new document will cause some of the internal caching
to be reset.

Conditions that cause a result Err with message.
* Error there are no documents in index to search.
* Error query is empty.
* Error after tokenisation there are no terms to search for.

-}
search :
       String
    -> Index doc
    -> Result String (Index doc, List (String, Float))
search =
      Index.search


{-| Store an index to a Value.

You can also use [`ElmTextSearch.Json.Encoder`](ElmTextSearch.Json.Encoder).
-}
storeToValue : Index doc -> Encode.Value
storeToValue =
    IndexEncoder.encoder


{-| Store an index to a String.

You can also use [`ElmTextSearch.Json.Encoder`](ElmTextSearch.Json.Encoder).
-}
storeToString : Index doc -> String
storeToString index =
    Encode.encode 0 (IndexEncoder.encoder index)


{-| Create an Index from a String which has a stored Index in it and the
supplied basic configurations.

See [`ElmTextSearch.fromStringWith`](ElmTextSearch#fromStringWith) for possible Err results.
-}
fromString : SimpleConfig doc -> String -> Result String (Index doc)
fromString simpleConfig inputString =
    IndexLoad.loadIndex
      (getIndexSimpleConfig simpleConfig)
      inputString


{-| Create an Index from a Value which has a stored Index in it.

See [`ElmTextSearch.fromStringWith`](ElmTextSearch#fromStringWith) for possible Err results.
-}
fromValue : SimpleConfig doc -> Decode.Value -> Result String (Index doc)
fromValue simpleConfig inputValue =
    IndexLoad.loadIndexValue
      (getIndexSimpleConfig simpleConfig)
      inputValue


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
* "Error cannot load Index. Tried to load index of type \"__IndexTest Type -\". It is not in supported index configurations."
 * It contains the loaded version index type which comes from input.
* "Error cannot load Index. Version supported is 1.0.0. Version tried to load is 1.0.1."
 * It includes both expected and loaded versions which may vary.
-}
fromStringWith : List (Config doc) -> String -> Result String (Index doc)
fromStringWith =
    IndexLoad.loadIndexWith


{-| Create an Index from a String which has a stored Index in it.

If none of the indexVersion in the list of SimpleConfig match the index
being decoded it will return an Err.

See [`ElmTextSearch.fromStringWith`](ElmTextSearch#fromStringWith) for possible Err results.
-}
fromValueWith : List (Config doc) -> Decode.Value -> Result String (Index doc)
fromValueWith =
    IndexLoad.loadIndexValueWith
