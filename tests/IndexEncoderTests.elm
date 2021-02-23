module IndexEncoderTests exposing (testEncodeList, testEncoder)

import ElmTextSearch.Json.Encoder as IndexEncoder
import Expect
import Index
import Index.Model exposing (..)
import Json.Encode as Encode
import Test exposing (..)
import TestUtils


encodedIndex : String
encodedIndex =
    String.concat
        [ "{\"indexVersion\":\"1.1.0\",\"indexType\":\"- IndexTest Type -\","
        , "\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},"
        , "\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],"
        , "\"tokenStore\":{"
        , "\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},"
        , "\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},"
        , "\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},"
        , "\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},"
        , "\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},"
        , "\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
        ]


testEncoder : Test
testEncoder =
    test "Encode index with doc matches encodedIndex" <|
        \() ->
            Index.new
                { indexType = "- IndexTest Type -"
                , ref = .cid
                , fields = [ ( .title, 5 ), ( .body, 1 ) ]
                , listFields = []
                }
                |> Index.add
                    { cid = "doc1"
                    , title = "Examples of a Banana"
                    , author = "Sally Apples"
                    , body = "Sally writes words about a grown banana."
                    }
                |> TestUtils.getResultIgnoreError
                |> IndexEncoder.encoder
                |> Encode.encode 0
                |> Expect.equal
                    encodedIndex


testEncodeList : Test
testEncodeList =
    test "Encode index with doc matches encodedIndex using listFields" <|
        \() ->
            Index.new
                { indexType = "- IndexTest Type -"
                , ref = .cid
                , fields = [ ( .title, 5 ) ]
                , listFields = [ ( .body, 1 ) ]
                }
                |> Index.add
                    { cid = "doc1"
                    , title = "Examples of a Banana"
                    , author = "Sally Apples"
                    , body =
                        [ "Sally writes words "
                        , "about a grown banana."
                        ]
                    }
                |> TestUtils.getResultIgnoreError
                |> IndexEncoder.encoder
                |> Encode.encode 0
                |> Expect.equal
                    encodedIndex
