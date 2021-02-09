module IndexEncoderTests exposing (IndexResult, IndexResultList, MyDoc, MyDocList, doc1, doc1List, encodedIndex, encoder1, encoder1List, index0, index0List, index1, index1List, safeIndex, safeIndexList, tests)

import ElmTextSearch.Json.Encoder as IndexEncoder
import Expect
import Index
import Index.Model exposing (..)
import Json.Encode as Encode
import Test exposing (..)


tests : Test
tests =
    describe "Index Encoder tests"
        [ encoder1 ()
        , encoder1List ()
        ]


{-| example record type for tests
-}
type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


type alias MyDocList =
    { cid : String
    , title : String

    --, subTitle : String
    , author : String
    , body : List String

    --, bodyExtra : List String
    }


type alias IndexResult =
    Result String (Index MyDoc)


type alias IndexResultList =
    Result String (Index MyDocList)


safeIndex : (() -> IndexResult) -> Index MyDoc
safeIndex result =
    Result.withDefault index0 (result ())


safeIndexList : (() -> IndexResultList) -> Index MyDocList
safeIndexList result =
    Result.withDefault index0List (result ())


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


{-| example index
-}
index0List : Index MyDocList
index0List =
    Index.new
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields =
            [ ( .title, 5 )

            --, ( .subTitle, 3 )
            ]
        , listFields =
            [ ( .body, 1 )

            --, ( .bodyExtra, 2 )
            ]
        }


index1 : () -> IndexResult
index1 _ =
    Index.add (doc1 ()) index0


index1List : () -> IndexResultList
index1List _ =
    Index.add (doc1List ()) index0List


doc1 : () -> MyDoc
doc1 _ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


doc1List : () -> MyDocList
doc1List _ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body =
        [ "Sally writes words "
        , "about a grown banana."
        ]
    }


encodedIndex =
    "{\"indexVersion\":\"1.1.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


encoder1 _ =
    test "Encode an index " <|
        \() ->
            Expect.equal
                encodedIndex
                (Encode.encode 0
                    (IndexEncoder.encoder (safeIndex index1))
                )


encoder1List _ =
    test "Encode an index with listFields body same content as encoder1 test " <|
        \() ->
            Expect.equal
                encodedIndex
                (Encode.encode 0
                    (IndexEncoder.encoder (safeIndexList index1List))
                )
