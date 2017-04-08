module Index.Model exposing (..)

{-| Define the Index Model

@docs Index
@docs IndexSimpleConfig
@docs IndexConfig

Copyright (c) 2016 Robin Luiten

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Trie exposing (Trie)


{-| Func and Factory types used with ElmTextSearch.
-}
type alias FuncFactory doc func =
    Index doc -> ( Index doc, func )


type alias TransformFunc =
    String -> String


type alias TransformFactory doc =
    Index doc -> ( Index doc, String -> String )


type alias FilterFunc =
    String -> Bool


type alias FilterFactory doc =
    Index doc -> ( Index doc, String -> Bool )


{-| Index is a full text index for a document type.

The internal data model of Index

  - indexType
      - a string that can be used on load to provide the correct set

  - indexVersion
      - a version string

  - ref
      - how to get at unique id of documents added

  - fields
      - list of fields of type String to index from document
          - first field is function to get String content of field
          - second field Float is a boost to text frequency of tokens in this field

  - listFields
      - list of fields of type List String to index from document
          - first field is function to get List String content of field
          - second field Float is a boost to text frequency of tokens in this field

  - transformFactories
      - list of factory functions to create transform functions

  - filterFactories
      - list of factory functions to create filter functions

  - transforms
      - the transforms in index token processing
      - lazy populated from transformFactorys

  - filters
      - the files in index token processing
      - lazy populated from transformFactorys

  - documentStore
      - contains dictionary of document ref to Set of document tokens

  - corpusTokens
      - Set of all indexed tokens from all documentStore

  - corpusTokensIndex
      - to get the position of a token in the order list of corpusTokens

  - tokenStore
      - tokenStore is used for efficient storing and lookup of the
        reverse index of token to document ref and holding the
        token term frequency

  - idfCache
      - cached idf (inverse document frequency scores)
      - these are cleared if a document is added removed or updated

-}
type Index doc
    = Index (IndexRecord doc)


{-| The Record model in an Index.
-}
type alias IndexRecord doc =
    { indexVersion : String
    , indexType : String
    , ref : doc -> String
    , fields : List ( doc -> String, Float )
    , listFields : List ( doc -> List String, Float )
    , transformFactories : List (TransformFactory doc)
    , filterFactories : List (FilterFactory doc)
    , documentStore : Dict String (Set String)
    , corpusTokens : Set String
    , tokenStore : Trie Float
    , corpusTokensIndex : Dict String Int
    , transforms : Maybe (List TransformFunc)
    , filters : Maybe (List FilterFunc)
    , idfCache : Dict String Float
    }


{-| Simple index config with default token processing.

Simple still requires configuring the fields for your document type.
See [`ElmTextSearch.SimpleConfig`](ElmTextSearch#SimpleConfig)
for explantions of `ref`, `fields` and `listFields` fields.

  - ElmTextSearch.SimpleConfig does not include `indexType`.
      - In this case the user is getting the ElmTextSearch default token processing.
  - Index.SimpleConfig includes `indexType`.

`indexType` is an identifier used to determine the transforms and filters the
index uses for operation. It should be unique for all possible differently
configured indexes you plan to use.


### The default transform factories.

    Index.Defaults.defaultTransformFactories


### The default filter factories.

    Index.Defaults.defaultFilterFactories

-}
type alias SimpleConfig doc =
    { indexType : String
    , ref : doc -> String
    , fields : List ( doc -> String, Float )
    , listFields : List ( doc -> List String, Float )
    }


{-| Index config with customized token processing.

If a configuration does not match an index being loaded
you will get an Err Result returned.

-}
type alias Config doc =
    { indexType : String
    , ref : doc -> String
    , fields : List ( doc -> String, Float )
    , listFields : List ( doc -> List String, Float )
    , transformFactories : List (TransformFactory doc)
    , filterFactories : List (FilterFactory doc)
    }


{-| Just the fields encoded for an Index.
-}
type alias CodecIndexRecord =
    { indexVersion : String
    , indexType : String
    , documentStore : Dict String (Set String)
    , corpusTokens : Set String
    , tokenStore : Trie Float
    }
