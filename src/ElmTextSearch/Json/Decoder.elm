module ElmTextSearch.Json.Decoder (decoder) where

{-| Decoder for Index.

It decodes to a CodecIndexRecord.

@docs decoder

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)
import Trie.Json.Decoder as TrieDecoder

import IndexModel
import Utils exposing ((|:))


{-| CodecIndexRecord decoder. -}
decoder : Decoder IndexModel.CodecIndexRecord
decoder =
    succeed IndexModel.CodecIndexRecord
      |: ("indexVersion" := string)
      |: ("indexType" := string)
      |: ("documentStore" := documentStoreDecoder)
      |: ("corpusTokens" := setDecoder)
      |: ("tokenStore" := TrieDecoder.decoder float)


documentStoreDecoder : Decoder (Dict String (Set String))
documentStoreDecoder =
    dict setDecoder


setDecoder : Decoder (Set String)
setDecoder =
    map Set.fromList (list string)
