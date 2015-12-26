module Lunrelm
    ( Index
    , SimpleConfig
    , Config
    , new
    , newWith
    , load
    , loadWith
    , save
    , add
    , remove
    , update
    , search
    ) where

{-| Lunrelm a full text indexer written in Elm language inspired by lunr.js.

Copyright (c) 2016 Robin Luiten

A useful article about lunr.js

https://www.new-bamboo.co.uk/blog/2013/02/26/full-text-search-in-your-browser/


## Create
@docs new
@docs newWith

## Save and load
@docs load
@docs loadWith
@docs save

## Modify
@docs add
@docs remove
@docs update

## Query
@docs search

## Types
@docs Index
@docs Config
@docs SimpleConfig


-}

import Index
import IndexModel


{-| The type of index Lunrelm defaults to using.
It defines the default token transforms and filters.

-}
lunrelmIndexType : String
lunrelmIndexType = "_.oO() Lunrelm Default ()Oo._"


{-| Lunrelm Index
-}
type alias Index doc = Index.Index doc


{-| Simple Lunrelm Config. -}
type alias SimpleConfig doc a =
    { a
    | ref : (doc -> String)
    , fields : List (doc -> String, Float)
    }


{-| Lunrelm configuration. -}
type alias Config doc a = IndexModel.Config doc a


{-| Create new Lunrelm index.

Example

```
get from examples/LunrelmNew.elm
```

### The following are the default Lunrelm Transformer functions.

```
createDefaultTransformerFunc1 = (IndexUtils.createFuncFactory TokenProcessors.trim)
```

```
createDefaultTransformerFunc2 = (IndexUtils.createFuncFactory Stemmer.stem)
```

### The following are the default Lunrelm Filter functions

```
* StopWordFilter.createDefaultFilterFunc
```

-}
new : SimpleConfig doc a -> Index doc
new {ref, fields} =
  Index.new
    { indexType = lunrelmIndexType
    , ref = ref
    , fields = fields
    }


{-| Create new Lunrelm index with additional configuration.

```
get from examples/LunrelmNewWith.elm
```

### Performance considerations.

* It is more efficient to merge stop word filters based on lists
of words into a single list of words and make a single stop word
filter out of them.
 * StopWordFilter.createDefaultFilterFunc
 * StopWordFilter.createFilterFuncWith
 * StopWordFilter.createFilterFunc

-}
newWith : Config doc a -> Index doc
newWith = Index.newWith


{-| Load a Lunrelm index.
TODO finish, currently returns new index.
-}
load : List (SimpleConfig doc a) -> String -> Result String (Index doc)
load simpleConfigs data =
    Err "load is not implemented"
    -- case simpleConfigs of
    --   [] -> Err "Error no configuration provided to load."
    --   headConfig :: tail -> Ok (new headConfig)


{-| Load custom Lunrelm index.
TODO finish
-}
loadWith : List (Config doc a) -> String -> Result String (Index doc)
loadWith configs data =
    Err "loadWith is not implemented"
    -- case configs of
    --   [] -> Err "Error no configuration provided to load."
    --   headConfig :: tail -> Ok (newWith headConfig)


{-| Save an index to a string.

Is some other way better than String output ?

TODO finish
-}
save : Index doc -> String
save index = Debug.crash("save not implmented yet")


{-| Add a document to Lunrelm index.

Starting with the Lunrelm.new example above this adds a document to
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

Conditions that cause a result Err with message.
* Error document has an empty unique id (ref).
* Error document is not in index.
-}
remove : Index doc -> doc -> Result String (Index doc)
remove = Index.remove


{-| Update a doc in Lunrelm index.

Conditions that cause an error result are those for `remove` and then `add`.
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

The following example runs a search for documents containing both "foo" and "bar".

```
    (updatedMyIndex, results) = Index.search myIndex "foo bar"
```

Results are a list of matching document reference identifiers with
there similarity to query score, ordered by score ascending.

Conditions that cause a result Err with message.
* Error there are no documents in index to search.
* Error query is empty.
* Error after tokenisation there are no terms to search for.

-}
search : Index doc -> String -> Result String (Index doc, List (String, Float))
search = Index.search
