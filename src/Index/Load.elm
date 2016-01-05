module Index.Load where

{-| Load an index from Value or String

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode
import Stemmer

import Index.Defaults as Defaults
import Index.Model exposing (..)
import Index.Utils
import ElmTextSearch.Json.Decoder as IndexDecoder
import StopWordFilter
import TokenProcessors


errorPrefix = "Error cannot load Index."

{-| Decode an index with one of provided configs.

The configurations supplied will be used in the order provided in
the list so the earliest one that matches indexType is used.
Try to use a supported index config first.
Then try the default just in case.

-}
loadIndexWith : List (Config doc) -> String -> Result String (Index doc)
loadIndexWith supportedIndexConfigs inputString =
    (Decode.decodeString IndexDecoder.decoder inputString)
      `Result.andThen` checkIndexVersion
      `Result.andThen` (checkIndexType supportedIndexConfigs)
      `Result.andThen` loadIndexFull


loadIndexValueWith : List (Config doc) -> Decode.Value -> Result String (Index doc)
loadIndexValueWith supportedIndexConfigs inputValue =
    (Decode.decodeValue IndexDecoder.decoder inputValue)
      `Result.andThen` checkIndexVersion
      `Result.andThen` (checkIndexType supportedIndexConfigs)
      `Result.andThen` loadIndexFull


checkIndexVersion : CodecIndexRecord -> Result String (CodecIndexRecord)
checkIndexVersion decodedIndex =
    if Defaults.indexVersion == decodedIndex.indexVersion then
      Ok decodedIndex
    else
      Err (errorPrefix ++ " Version supported is "
          ++ Defaults.indexVersion ++ ". Version tried to load is "
          ++ decodedIndex.indexVersion ++ ".")

checkIndexType :
       List (Config doc)
    -> CodecIndexRecord
    -> Result String (Config doc, CodecIndexRecord)
checkIndexType supportedIndexConfigs decodedIndex =
    let
      config =
        List.filter
          (\cfg -> cfg.indexType == decodedIndex.indexType)
          supportedIndexConfigs
    in
      case config of
        [] ->
          Err (errorPrefix ++ " Tried to load index of type \""
              ++ decodedIndex.indexType
              ++ "\". It is not in supported index configurations.")

        matchedConfig :: _ ->
          Ok (matchedConfig, decodedIndex)


loadIndexFull : (Config doc, CodecIndexRecord) -> Result String (Index doc)
loadIndexFull (config, decodedIndex) =
    Ok <|
      Index
        { indexVersion = decodedIndex.indexVersion
        , indexType = decodedIndex.indexType
        , ref = config.ref
        , fields = config.fields
        , transformFactories = config.transformFactories
        , filterFactories = config.filterFactories
        , documentStore = decodedIndex.documentStore
        , corpusTokens = decodedIndex.corpusTokens
        , tokenStore = decodedIndex.tokenStore
        , corpusTokensIndex =
            (Index.Utils.buildOrderIndex decodedIndex.corpusTokens)
        , transforms = Nothing
        , filters = Nothing
        , idfCache = Dict.empty
        }


loadIndex : SimpleConfig doc -> String -> Result String (Index doc)
loadIndex simpleConfig inputString =
    loadIndexWith
      [ Defaults.getDefaultIndexConfig simpleConfig ]
      inputString


loadIndexValue : SimpleConfig doc -> Decode.Value -> Result String (Index doc)
loadIndexValue simpleConfig inputValue =
    loadIndexValueWith
      [ Defaults.getDefaultIndexConfig simpleConfig ]
      inputValue
