module IndexUtilsTests exposing (..)

import Expect
import Test exposing (..)
import Index.Model
import Index exposing (Index)
import Index.Utils
import TokenProcessors
import Stemmer


type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
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


tests : Test
tests =
    describe "Index.Utils tests"
        [ describe "apply default transform tests"
            (List.map testDefaultTransforms defaultTransformCases)
        ]


defaultTransformCases =
    [ ( "words of only non word chars removed"
      , "engineering ???"
      , [ "engin" ]
      )
    , ( "stemmer and non word chars removed"
      , ".This was very large.-"
      , [ "veri", "larg" ]
      )
    , ( "stop words removed"
      , "however among the dear .- -"
      , []
      )
    ]


testDefaultTransforms ( name, input, expected ) =
    let
        a =
            1
    in
        test ("getTokens \"" ++ input ++ "\" " ++ name) <|
            \() ->
                Expect.equal
                    expected
                    (Tuple.second (Index.Utils.getTokens index0 input))
