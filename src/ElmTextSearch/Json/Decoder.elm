module ElmTextSearch.Json.Decoder exposing (decoder)

{-| Decoder for Index.

It decodes to a CodecIndexRecord.

@docs decoder

Copyright (c) 2016-2017 Robin Luiten

-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required, decode)
import Set exposing (Set)
import Trie.Json.Decoder as TrieDecoder
import Index.Model as Model


{-| CodecIndexRecord decoder.
-}
decoder : Decoder Model.CodecIndexRecord
decoder =
    decode Model.CodecIndexRecord
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
