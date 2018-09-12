module ElmTextSearch.Json.Decoder exposing (decoder)

{-| Decoder for Index.

It decodes to a CodecIndexRecord.

@docs decoder

Copyright (c) 2016 Robin Luiten

-}

import Dict exposing (Dict)
import Index.Model as Model
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Set exposing (Set)
import Trie.Json.Decoder as TrieDecoder


{-| CodecIndexRecord decoder.
-}
decoder : Decoder Model.CodecIndexRecord
decoder =
    Decode.succeed Model.CodecIndexRecord
        |> required "indexVersion" string
        |> required "indexType" string
        |> required "documentStore" documentStoreDecoder
        |> required "corpusTokens" setDecoder
        |> required "tokenStore" (TrieDecoder.decoder float)


documentStoreDecoder : Decoder (Dict String (Set String))
documentStoreDecoder =
    dict setDecoder


setDecoder : Decoder (Set String)
setDecoder =
    map Set.fromList (list string)
