module Index.Load exposing (errorPrefix, loadIndex, loadIndexValue, loadIndexValueWith, loadIndexWith)

{-| Load an index from Value or String

Copyright (c) 2016 Robin Luiten

-}

import Dict
import ElmTextSearch.Json.Decoder as IndexDecoder
import Index.Defaults as Defaults
import Index.Model exposing (..)
import Index.Utils
import Json.Decode as Decode


errorPrefix : String
errorPrefix =
    "Error cannot load Index."


{-| Decode an index with one of provided configs.

The configurations supplied will be used in the order provided in
the list so the earliest one that matches indexType is used.
Try to use a supported index config first.
Then try the default just in case.

-}
loadIndexWith : List (Config doc) -> String -> Result Decode.Error (Index doc)
loadIndexWith supportedIndexConfigs inputString =
    Decode.decodeString
        (IndexDecoder.decoder
            |> Decode.andThen (mapIndexConfig supportedIndexConfigs)
            |> Decode.andThen createIndex
        )
        inputString


mapIndexConfig : List (Config doc) -> CodecIndexRecord -> Decode.Decoder ( Config doc, CodecIndexRecord )
mapIndexConfig supportedIndexConfigs index =
    if Defaults.indexVersion /= index.indexVersion then
        Decode.fail <|
            (errorPrefix
                ++ " Version supported is "
                ++ Defaults.indexVersion
                ++ ". Version tried to load is "
                ++ index.indexVersion
                ++ "."
            )

    else
        let
            config =
                List.filter
                    (\cfg -> cfg.indexType == index.indexType)
                    supportedIndexConfigs
        in
        case config of
            [] ->
                Decode.fail <|
                    (errorPrefix
                        ++ " Tried to load index of type \""
                        ++ index.indexType
                        ++ "\". It is not in supported index configurations."
                    )

            matchedConfig :: _ ->
                Decode.succeed ( matchedConfig, index )


loadIndexValueWith : List (Config doc) -> Decode.Value -> Result Decode.Error (Index doc)
loadIndexValueWith supportedIndexConfigs inputValue =
    Decode.decodeValue
        (IndexDecoder.decoder
            |> Decode.andThen (mapIndexConfig supportedIndexConfigs)
            |> Decode.andThen createIndex
        )
        inputValue


createIndex : ( Config doc, CodecIndexRecord ) -> Decode.Decoder (Index doc)
createIndex ( config, decodedIndex ) =
    Decode.succeed <|
        Index
            { indexVersion = decodedIndex.indexVersion
            , indexType = decodedIndex.indexType
            , ref = config.ref
            , fields = config.fields
            , listFields = config.listFields
            , initialTransformFactories = config.initialTransformFactories
            , transformFactories = config.transformFactories
            , filterFactories = config.filterFactories
            , documentStore = decodedIndex.documentStore
            , corpusTokens = decodedIndex.corpusTokens
            , tokenStore = decodedIndex.tokenStore
            , corpusTokensIndex =
                Index.Utils.buildOrderIndex decodedIndex.corpusTokens
            , initialTransforms = Nothing
            , transforms = Nothing
            , filters = Nothing
            , idfCache = Dict.empty
            }


loadIndex : ModelSimpleConfig doc -> String -> Result Decode.Error (Index doc)
loadIndex simpleConfig inputString =
    loadIndexWith
        [ Defaults.getDefaultIndexConfig simpleConfig ]
        inputString


loadIndexValue : ModelSimpleConfig doc -> Decode.Value -> Result Decode.Error (Index doc)
loadIndexValue simpleConfig inputValue =
    loadIndexValueWith
        [ Defaults.getDefaultIndexConfig simpleConfig ]
        inputValue
