module IndexLoadTests exposing
    ( indexfromString1Test
    , loadIndexWith1Test
    , loadIndexWithErr1Test
    , loadIndexWithErr2Test
    )

import ElmTextSearch
import Expect
import Index
import Index.Load
import Index.Model exposing (Index(..))
import Json.Decode exposing (Error(..))
import Test exposing (..)
import TestUtils


loadIndexWithErr1Test : Test
loadIndexWithErr1Test =
    test "Fails to load an index with wrong index version" <|
        \() ->
            String.concat
                [ "{\"indexVersion\":\"1.0.1\",\"indexType\":\"- IndexTest Type -\","
                , "\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},"
                , "\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],"
                , "\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},"
                , "\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},"
                , "\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},"
                , "\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},"
                , "\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},"
                , "\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
                ]
                |> Index.Load.loadIndexWith
                    [ { indexType = "_______some string"
                      , ref = .cid
                      , fields = [ ( .title, 5 ), ( .body, 1 ) ]
                      , listFields = []
                      , initialTransformFactories = []
                      , transformFactories = []
                      , filterFactories = []
                      }
                    ]
                |> TestUtils.getErrorIgnoreResult
                |> TestUtils.getDecodeErrorFailureMessage
                |> Expect.equal "Error cannot load Index. Version supported is 1.1.0. Version tried to load is 1.0.1."


loadIndexWithErr2Test : Test
loadIndexWithErr2Test =
    test "Fails to load an index with an indexType not in configuration provided." <|
        \() ->
            String.concat
                [ "{\"indexVersion\":\"1.1.0\",\"indexType\":\"__IndexTest Type -\","
                , "\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},"
                , "\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],"
                , "\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},"
                , "\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},"
                , "\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},"
                , "\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},"
                , "\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},"
                , "\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
                ]
                |> Index.Load.loadIndexWith
                    [ { indexType = "_______some string not matching the encoded index type"
                      , ref = .cid
                      , fields = [ ( .title, 5 ), ( .body, 1 ) ]
                      , listFields = []
                      , initialTransformFactories = []
                      , transformFactories = []
                      , filterFactories = []
                      }
                    ]
                |> TestUtils.getErrorIgnoreResult
                |> TestUtils.getDecodeErrorFailureMessage
                |> Expect.equal "Error cannot load Index. Tried to load index of type \"__IndexTest Type -\". It is not in supported index configurations."


loadIndexWith1Test : Test
loadIndexWith1Test =
    let
        config =
            { indexType = "not set"
            , ref = .cid
            , fields = [ ( .title, 5 ), ( .body, 1 ) ]
            , listFields = []
            , initialTransformFactories = []
            , transformFactories = []
            , filterFactories = []
            }
    in
    test "Load an index. really dumb check" <|
        \() ->
            String.concat
                [ "{\"indexVersion\":\"1.1.0\",\"indexType\":\"_______some string\","
                , "\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},"
                , "\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],"
                , "\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},"
                , "\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},"
                , "\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},"
                , "\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},"
                , "\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},"
                , "\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
                ]
                |> Index.Load.loadIndexWith
                    [ config
                    , { config | indexType = "_______some string" }
                    ]
                |> TestUtils.expectOkWithGoodFailMessage


indexfromString1Test : Test
indexfromString1Test =
    test "Can succesfully load index from string with ElmTextSearch.SimpleConfig." <|
        \() ->
            String.concat
                [ "{\"indexVersion\":\"1.1.0\",\"indexType\":\"-= ElmTextSearch Index Type 1 =-\","
                , "\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},"
                , "\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],"
                , "\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},"
                , "\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},"
                , "\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},"
                , "\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},"
                , "\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},"
                , "\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
                ]
                |> ElmTextSearch.fromString
                    { ref = .cid
                    , fields =
                        [ ( .title, 5 )
                        , ( .body, 1 )
                        ]
                    , listFields = []
                    }
                |> TestUtils.expectOkWithGoodFailMessage
