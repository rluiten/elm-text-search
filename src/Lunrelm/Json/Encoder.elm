module Lunrelm.Json.Encoder (encoder, codecIndexRecordEncoder) where

{-| Encoder for Lunrelm Index.

@docs encoder
@docs codecIndexRecordEncoder

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing (Dict)
import Json.Encode as Encode
import Set exposing (Set)
import Trie exposing (Trie)
import Trie.Json.Encoder as TrieEncoder

import Index
import IndexModel exposing (..)


{-| Encoder for Index a.

Only encoding fields required to recreate a working index.

The following fields are not saved as they are restored via
the provided Config on fromString.
* ref
* fields
* transformFactories
* filterFactories

The following fields are not saved because they are an
acceleration model, decoder needs to set it on fromString.
* corpusTokensIndex

The following fields are not saved because they are caches
and are cached as operationg requires
* transforms
* filters
* idfCache

Do not need an (a -> Encode.Value) because a is a document
type and that is never encoded from an Index.
-}
encoder : (Index doc) -> Encode.Value
encoder (Index irec) =
    codecIndexRecordEncoder
      { indexVersion = irec.indexVersion
      , indexType = irec.indexType
      , documentStore = irec.documentStore
      , corpusTokens = irec.corpusTokens
      , tokenStore = irec.tokenStore
      }


{-| Encode CodecIndexRecord. -}
codecIndexRecordEncoder : CodecIndexRecord -> Encode.Value
codecIndexRecordEncoder rec =
    Encode.object
      [ ("indexVersion", Encode.string rec.indexVersion)
      , ("indexType", Encode.string rec.indexType)
      , ("documentStore", documentStoreEncoder rec.documentStore)
      , ("corpusTokens", corpusTokensEncoder rec.corpusTokens)
      , ("tokenStore", tokenStore rec.tokenStore)
      ]


documentStoreEncoder : (Dict String (Set String)) -> Encode.Value
documentStoreEncoder dict =
    Encode.object <|
      List.map
        (\(key, val) ->
          ( key
          , Encode.list
            (List.map Encode.string (Set.toList val))
          )
        )
        (Dict.toList dict)


corpusTokensEncoder : Set String -> Encode.Value
corpusTokensEncoder setVal =
    Encode.list (List.map Encode.string (Set.toList setVal))


tokenStore : Trie Float -> Encode.Value
tokenStore = TrieEncoder.encoder Encode.float
