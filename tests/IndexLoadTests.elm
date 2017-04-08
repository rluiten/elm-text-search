module IndexLoadTests exposing (..)

import Dict
import Expect
import Test exposing (..)
import String
import Stemmer
import ElmTextSearch
import Index
import Index.Model exposing (Index(..))
import Index.Utils
import Index.Load
import StopWordFilter
import TokenProcessors
import TestUtils exposing (expectOk)


tests : Test
tests =
    describe "LoadIndex tests"
        [ loadIndexWithErr1 ()
        , loadIndexWithErr2 ()
        , loadIndexWith1 ()
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
    , transformFactories =
        [ Index.Utils.createFuncCreator TokenProcessors.trimmer
        , Index.Utils.createFuncCreator Stemmer.stem
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
    , transformFactories =
        [ Index.Utils.createFuncCreator TokenProcessors.trimmer
        , Index.Utils.createFuncCreator Stemmer.stem
        ]
    , filterFactories =
        [ createMyStopWordFilter
        ]
    }


{-| encoded variants for testing load
-}
exampleJsonIndex100 =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"__IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex101 =
    "{\"indexVersion\":\"1.0.1\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex100somestring =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"_______some string\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


exampleJsonIndex100default =
    "{\"indexVersion\":\"1.0.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


loadIndexWithErr1 _ =
    test "Fails to load an index with non indexVersion." <|
        \() ->
            Expect.equal (Err ("Error cannot load Index. Version supported is 1.0.0. Version tried to load is 1.0.1.")) <|
                Index.Load.loadIndexWith
                    [ config1 ]
                    exampleJsonIndex101


loadIndexWithErr2 _ =
    test "Fails to load an index with an indexType not in configuration provided." <|
        \() ->
            Expect.equal (Err ("Error cannot load Index. Tried to load index of type \"__IndexTest Type -\". It is not in supported index configurations.")) <|
                Index.Load.loadIndexWith
                    [ config1 ]
                    exampleJsonIndex100


loadIndexWith1 _ =
    test "Load an index. really dumb check" <|
        \() ->
            expectOk <|
                Index.Load.loadIndexWith
                    [ config2
                    , config1
                    ]
                    exampleJsonIndex100somestring


indexfromString1 _ =
    test "I can load index from sting with ElmTextSearch.SimpleConfig." <|
        \() ->
            expectOk <|
                ElmTextSearch.fromString
                    { ref = .cid
                    , fields =
                        [ ( .title, 5 )
                        , ( .body, 1 )
                        ]
                    , listFields = []
                    }
                    exampleJsonIndex100default
