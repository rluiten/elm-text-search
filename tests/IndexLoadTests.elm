module IndexLoadTests exposing (IndexAndListResult, IndexResult, MyDoc, config1, config2, createMyStopWordFilter, doc1, exampleJsonIndex100, exampleJsonIndex100default, exampleJsonIndex100somestring, exampleJsonIndex101, index0, indexfromString1, jsonIndexDefault, loadIndexWith1, loadIndexWithErr1, loadIndexWithErr2, myStopWords, tests)

import Dict
import ElmTextSearch
import Expect
import Index
import Index.Defaults
import Index.Load
import Index.Model exposing (Index(..))
import Index.Utils
import Json.Decode as Decode
import Stemmer
import StopWordFilter
import String
import Test exposing (..)
import TestUtils exposing (expectErr, expectOk, expectResultFailureMessage, mapDecodeErrorToString)
import TokenProcessors


tests : Test
tests =
    describe "LoadIndex tests"
        [ loadIndexWithErr1 ()
        , loadIndexWithErr2 ()
        , loadIndexWith1 ()
        , indexfromString1 ()
        ]


{-| example record type for tests
-}
type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


doc1 : () -> MyDoc
doc1 _ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


{-| example index
-}
index0 : Index MyDoc
index0 =
    Index.new
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields =
            [ ( .title, 5 )
            , ( .body, 1 )
            ]
        , listFields = []
        }


type alias IndexResult =
    Result String (Index MyDoc)


type alias IndexAndListResult =
    Result String ( Index MyDoc, List ( String, Float ) )


myStopWords =
    [ "electronic", "harvesting", "pawpaw" ]


createMyStopWordFilter =
    StopWordFilter.createFilterFuncWith myStopWords


config1 =
    { indexType = "_______some string"
    , ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    , listFields = []
    , initialTransformFactories =
        [ Index.Utils.createFuncCreator TokenProcessors.trimmer ]
    , transformFactories =
        [ Index.Utils.createFuncCreator Stemmer.stem
        ]
    , filterFactories =
        [ createMyStopWordFilter
        ]
    }


config2 =
    { indexType = "@@@@@@@some string"
    , ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    , listFields = []
    , initialTransformFactories =
        [ Index.Utils.createFuncCreator TokenProcessors.trimmer ]
    , transformFactories =
        [ Index.Utils.createFuncCreator Stemmer.stem
        ]
    , filterFactories =
        [ createMyStopWordFilter
        ]
    }


{-| encoded variants for testing load
-}
exampleJsonIndex100 =
    "{\"indexVersion\":\"1.1.0\",\"indexType\":\"__IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex101 =
    "{\"indexVersion\":\"1.0.1\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex100somestring =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"_______some string\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex100default =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


jsonIndexDefault =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"__IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


loadIndexWithErr1 _ =
    expectResultFailureMessage
        "Fails to load an index with wrong index version."
        "Error cannot load Index. Version supported is 1.1.0. Version tried to load is 1.0.1."
    <|
        Index.Load.loadIndexWith
            [ config1 ]
            exampleJsonIndex101


loadIndexWithErr2 _ =
    expectResultFailureMessage
        "Fails to load an index with an indexType not in configuration provided."
        "Error cannot load Index. Tried to load index of type \"__IndexTest Type -\". It is not in supported index configurations."
    <|
        Index.Load.loadIndexWith
            [ config1 ]
            exampleJsonIndex100


loadIndexWith1 _ =
    test "Load an index. really dumb check" <|
        \() ->
            expectOk <|
                mapDecodeErrorToString
                    (Index.Load.loadIndexWith
                        [ config2
                        , config1
                        ]
                        exampleJsonIndex100somestring
                    )


indexfromString1 _ =
    test "I can load index from string with ElmTextSearch.SimpleConfig." <|
        \() ->
            expectOk <|
                mapDecodeErrorToString
                    (ElmTextSearch.fromString
                        { ref = .cid
                        , fields =
                            [ ( .title, 5 )
                            , ( .body, 1 )
                            ]
                        , listFields = []
                        }
                        exampleJsonIndex100default
                    )
