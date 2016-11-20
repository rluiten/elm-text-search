module IndexDecoderTests exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)

import ElmTextSearch.Json.Decoder as IndexDecoder
import ElmTextSearch.Json.Encoder as IndexEncoder


tests : Test
tests =
  describe "Index Decoder tests"
    [ decodeIndex1 ()
    ]


-- got from the encoder tests.
exampleJsonIndex1 =
  "{\"indexVersion\":\"1.0.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}"


{-
From http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Dict
QUOTE: "Dictionary equality with (==) is unreliable and should not be used."

Therefore decode then encode back to string to check its same.
-}
decodeIndex1 _ =
  let
    decodedIndexResult =
      Decode.decodeString IndexDecoder.decoder exampleJsonIndex1
    resultStr = Result.map
      (\decodedIndex ->
        Encode.encode 0 (IndexEncoder.codecIndexRecordEncoder decodedIndex)
      )
      decodedIndexResult
    -- _ = Debug.log ("decodeIndex1") (resultStr)
  in
    test "decode exampleJsonIndex1" <|
      \() ->
        Expect.equal
          (Ok "{\"indexVersion\":\"1.0.0\",\"indexType\":\"- IndexTest Type -\",\"documentStore\":{\"doc1\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"]},\"corpusTokens\":[\"banana\",\"exampl\",\"grown\",\"salli\",\"word\",\"write\"],\"tokenStore\":{\"b\":{\"a\":{\"n\":{\"a\":{\"n\":{\"a\":{\"doc1\":2.7}}}}}},\"e\":{\"x\":{\"a\":{\"m\":{\"p\":{\"l\":{\"doc1\":2.5}}}}}},\"g\":{\"r\":{\"o\":{\"w\":{\"n\":{\"doc1\":0.2}}}}},\"s\":{\"a\":{\"l\":{\"l\":{\"i\":{\"doc1\":0.2}}}}},\"w\":{\"o\":{\"r\":{\"d\":{\"doc1\":0.2}}},\"r\":{\"i\":{\"t\":{\"e\":{\"doc1\":0.2}}}}}}}")
          resultStr
