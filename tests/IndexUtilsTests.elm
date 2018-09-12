module IndexUtilsTests exposing (MyDoc, getTokensCases, index0, index1removeLast3Transform, testApplyTransform, testGetTokens, tests)

import Expect
import Index exposing (Index)
import Index.Model
import Index.Utils
import Stemmer
import Test exposing (..)
import TokenProcessors


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
            (List.map testGetTokens getTokensCases)
        , testApplyTransform
        ]


getTokensCases =
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
    , ( "\"on\" in the stop word list should not filter \"one\" (https://github.com/rluiten/elm-text-search/issues/10)"
      , "one two three"
        -- note that "one" is transformed to "on" by stemmer
      , [ "on", "two", "three" ]
      )
    ]


testGetTokens ( name, input, expected ) =
    test ("getTokens \"" ++ input ++ "\" " ++ name) <|
        \() ->
            Expect.equal
                expected
                (Tuple.second (Index.Utils.getTokens index0 input))


{-| A test index, to ensure all transform factories are applied.
-}
index1removeLast3Transform : Index MyDoc
index1removeLast3Transform =
    let
        removeLastCharFuncCreator =
            Index.Utils.createFuncCreator (String.dropRight 1)
    in
    Index.newWith
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields =
            [ ( .title, 5 )
            , ( .body, 1 )
            ]
        , listFields = []
        , initialTransformFactories = []
        , transformFactories =
            [ removeLastCharFuncCreator
            , removeLastCharFuncCreator
            ]
        , filterFactories = []
        }


testApplyTransform =
    test "test applyTransform applies all to words input" <|
        \() ->
            Expect.equal
                [ "wor", "testi" ]
                (Tuple.second <|
                    Index.Utils.applyTransform index1removeLast3Transform
                        [ "words", "testing", "a" ]
                )
