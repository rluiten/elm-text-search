module IndexModel where

{-| Define the Index Model

Copyright (c) 2016 Robin Luiten


@docs Index
@docs IndexSimpleConfig
@docs IndexConfig

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Trie exposing (Trie)


{- Func and Factory types used with Lunrelm. -}
type alias FuncFactory doc func = Index doc -> (Index doc, func)
type alias TransformFunc = String -> String
type alias TransformFactory doc = Index doc -> (Index doc, (String -> String))
type alias FilterFunc = String -> Bool
type alias FilterFactory doc = Index doc -> (Index doc, (String -> Bool))

--type alias FilterFactory doc = Index doc -> (String -> Bool)

{-| Index is a full text index for a document type.

The internal data model of Index
* indexType
 * a string that can be used on load to provide the correct set
* indexVersion
 * a version string
* ref
 * how to get at unique id of documents added
* fields
 * list of fields to index from document
  * first field is function to get content of field
  * second field Float is a boost to text frequency of tokens in this field

* transformFactories
 * list of factory functions to create transform functions
* filterFactories
 * list of factory functions to create filter functions

* transforms
 * the transforms in index token processing
 * lazy populated from transformFactorys
* filters
 * the files in index token processing
 * lazy populated from transformFactorys

* documentStore
 * contains dictionary of document ref to Set of document tokens

* corpusTokens
 * Set of all indexed tokens from all documentStore
* corpusTokensIndex
 * to get the position of a token in the order list of corpusTokens
* tokenStore
 * tokenStore is used for efficient storing and lookup of the
   reverse index of token to document ref and holding the
   token term frequency
* idfCache
 * cached idf (inverse document frequency scores)
 * these are cleared if a document is added removed or updated

-}
type Index doc =
    Index
      { indexVersion : String

      , indexType : String
      , ref : doc -> String
      , fields : List (doc -> String, Float)
      , transformFactories : List (TransformFactory doc)
      , filterFactories : List (FilterFactory doc)

      , transforms : Maybe (List TransformFunc)
      , filters : Maybe (List FilterFunc)

      , documentStore : Dict String (Set String)
      , corpusTokens : Set String
      , corpusTokensIndex : Dict String Int
      , tokenStore : Trie Float
      , idfCache : Dict String Float
      }


{-| Simple index config with default token processing.

Simple still requires configuring the fields for your document type.
-}
type alias SimpleConfig doc a =
    { a
    | indexType : String
    , ref : (doc -> String)
    , fields : List (doc -> String, Float)
    }


{-| Index config with customized token processing.

If a configuration does not match an index being loaded
you will get an Err Result returned.
-}
type alias Config doc a =
    { a
    | indexType : String
    , ref : doc -> String
    , fields : List (doc -> String, Float)
    , transformFactories : List (TransformFactory doc)
    , filterFactories : List (FilterFactory doc)
    }
