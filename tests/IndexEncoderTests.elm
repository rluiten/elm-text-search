module IndexEncoderTests where

import Dict
import ElmTest exposing (..)
import Json.Encode as Encode
import String

import Index
import Index.Model as Model exposing (..)
import ElmTextSearch.Json.Encoder as IndexEncoder


tests : Test
tests =
    suite "Index Encoder tests"
      [ encoder1 ()
      ]


-- example record type for tests
type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


type alias IndexResult = Result String (Index MyDoc)


safeIndex : (() -> IndexResult) -> Index MyDoc
safeIndex result = Result.withDefault index0 (result ())


-- example index
index0 : Index MyDoc
index0 =
    Index.new
      { indexType = "- IndexTest Type -"
      , ref = .cid
      , fields =
          [ ( .title, 5 )
          , ( .body, 1 )
          ]
      }

index1 : () -> IndexResult
index1 _ = Index.add (doc1 ()) index0


doc1 : () -> MyDoc
doc1 _ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


-- _ = Debug.log("index1") (index1)
-- _ = Debug.log("encoder index1")
--     (
--       let
--         a = 1
--         encodedTrie = Encode.encode 0
--             (IndexEncoder.encoder (safeIndex index1))
--
--       in
--         encodedTrie
--     )


encoder1 _ =
    test "Encode an index" <|
      assertEqual
        "{\"indexVersion\":\"1.0.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"
        ( Encode.encode 0
            (IndexEncoder.encoder (safeIndex index1))
        )

-- tweaked version of encdoer1 string to look at.
a =
  """
    "{"indexVersion":"1.0.0"
    ,"indexType":"- IndexTest Type -"
    ,"documentStore":{"doc1":["banana","exampl","grown","salli","word","write"]}
    ,"corpusTokens":["banana","exampl","grown","salli","word","write"]
    ,"tokenStore":{"b":{"a":{"n":{"a":{"n":{"a":{"doc1":2.7}}}}}},"e":{"x":{"a":{"m":{"p":{"l":{"doc1":2.5}}}}}},"g":{"r":{"o":{"w":{"n":{"doc1":0.2}}}}},"s":{"a":{"l":{"l":{"i":{"doc1":0.2}}}}},"w":{"o":{"r":{"d":{"doc1":0.2}}},"r":{"i":{"t":{"e":{"doc1":0.2}}}}}}}"
  """
