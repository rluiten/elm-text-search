module Index.Defaults exposing
    ( indexVersion
    , elmTextSearchIndexType
    , defaultTransformFactories
    , defaultFilterFactories
    , defaultTokenTrimmerFuncCreator
    , defaultStemmerFuncCreator
    , defaultStopWordFilterFuncCreator
    , defaultInitialTransformFactories
    , getDefaultIndexConfig
    , getIndexSimpleConfig
    )

{-| Defaults for indexes and configurations.


## Index Storage Engine Version and Type

@docs indexVersion
@docs elmTextSearchIndexType


## Built in Transforms and Filters

@docs defaultTransformFactories
@docs defaultFilterFactories
@docs defaultTokenTrimmerFuncCreator
@docs defaultStemmerFuncCreator
@docs defaultStopWordFilterFuncCreator
@docs defaultInitialTransformFactories


## Config type adapters

@docs getDefaultIndexConfig
@docs getIndexSimpleConfig

Copyright (c) 2016 Robin Luiten

-}

import Index.Model as Model
    exposing
        ( FilterFactory
        , IndexSimpleConfig
        , TransformFactory
        )
import Index.Utils
import Stemmer
import StopWordFilter
import TokenProcessors


{-| The version of index, for loading a saved index.

This is not the same as package version.

This needs to change if the encoded format changes. Be careful of updates to
Trie package, if Trie encoding format changes this version needs to change as
well.

-}
indexVersion : String
indexVersion =
    "1.1.0"


{-| The type of index defaults to using.
It defines the default token transforms and filters.
-}
elmTextSearchIndexType : String
elmTextSearchIndexType =
    "-= ElmTextSearch Index Type 1 =-"


{-| Index default transform factories.
-}
defaultTransformFactories : List (TransformFactory doc)
defaultTransformFactories =
    [ defaultStemmerFuncCreator
    ]


{-| Index default transform factories that apply before filters.
-}
defaultInitialTransformFactories : List (TransformFactory doc)
defaultInitialTransformFactories =
    [ defaultTokenTrimmerFuncCreator
    ]


{-| Index default filter factories.
-}
defaultFilterFactories : List (FilterFactory doc)
defaultFilterFactories =
    [ defaultStopWordFilterFuncCreator
    ]


{-| The default token trimmer transform function creator.
Normally applied first in transform functions.
-}
defaultTokenTrimmerFuncCreator : TransformFactory doc
defaultTokenTrimmerFuncCreator =
    Index.Utils.createFuncCreator TokenProcessors.trimmer


{-| The default token stemmer transform function creator.
-}
defaultStemmerFuncCreator : TransformFactory doc
defaultStemmerFuncCreator =
    Index.Utils.createFuncCreator Stemmer.stem


{-| The default stop word filter function creator.
-}
defaultStopWordFilterFuncCreator : FilterFactory doc
defaultStopWordFilterFuncCreator =
    StopWordFilter.createDefaultFilterFunc


{-| Convert Index.Model.ModelSimpleConfig to Index.Model.Config
Filling in default values for fields not in SimpleConfig
This is the definition of the default index configuration.
-}
getDefaultIndexConfig : Model.ModelSimpleConfig doc -> Model.Config doc
getDefaultIndexConfig { indexType, ref, fields, listFields } =
    { indexType = indexType
    , ref = ref
    , fields = fields
    , listFields = listFields
    , initialTransformFactories = defaultInitialTransformFactories
    , transformFactories = defaultTransformFactories
    , filterFactories = defaultFilterFactories
    }


{-| convert ElmTextSearch.IndexSimpleConfig to Index.Model.ModelSimpleConfig
-}
getIndexSimpleConfig : IndexSimpleConfig doc -> Model.ModelSimpleConfig doc
getIndexSimpleConfig { ref, fields, listFields } =
    { indexType = elmTextSearchIndexType
    , ref = ref
    , fields = fields
    , listFields = listFields
    }
