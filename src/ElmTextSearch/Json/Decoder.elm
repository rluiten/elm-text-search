module ElmTextSearch.Json.Decoder exposing (decoder)

{-| Decoder for Index.

It decodes to a CodecIndexRecord.

@docs decoder

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)
import Trie.Json.Decoder as TrieDecoder

import Index.Model as Model
import Utils exposing ((|:))


{-| CodecIndexRecord decoder. -}
decoder : Decoder Model.CodecIndexRecord
decoder =
    succeed Model.CodecIndexRecord
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
