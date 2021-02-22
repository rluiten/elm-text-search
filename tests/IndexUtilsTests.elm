module IndexUtilsTests exposing
    ( testDefaultTransforms
    , testGetTokens
    , test_processTokens_filterFactories
    , test_processTokens_initialTransformFactories
    , test_processTokens_transformFactories
    )

import Expect
import Index exposing (Index)
import Index.Model exposing (FilterFactory, TransformFactory)
import Index.Utils
import StopWordFilter exposing (createFilterFunc)
import Test exposing (..)

type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


testDefaultTransforms : Test
testDefaultTransforms =
    describe "apply default transform tests"
        (List.map testGetTokens
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

            -- Bug https://github.com/rluiten/elm-text-search/issues/10
            , ( "\"on\" in the stop word list should not filter \"one\""
              , "one two three"
                -- note that "one" is transformed to "on" by stemmer
              , [ "on", "two", "three" ]
              )
            ]
        )


testGetTokens : ( String, String, List String ) -> Test
testGetTokens ( name, input, expected ) =
    test ("getTokens \"" ++ input ++ "\" " ++ name) <|
        \() ->
            let
                testMyDocIndex =
                    Index.new
                        { indexType = "- IndexTest Type -"
                        , ref = .cid
                        , fields =
                            [ ( .title, 5 )
                            , ( .body, 1 )
                            ]
                        , listFields = []
                        }
            in
            Index.Utils.getTokens
                testMyDocIndex
                input
                |> Tuple.second
                |> Expect.equal expected


createTestIndex1 :
    List (TransformFactory MyDoc)
    -> List (TransformFactory MyDoc)
    -> List (FilterFactory MyDoc)
    -> Index MyDoc
createTestIndex1 initialTransformFactories transformFactories filterFactories =
    Index.newWith
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields =
            [ ( .title, 5 )
            , ( .body, 1 )
            ]
        , listFields = []
        , initialTransformFactories = initialTransformFactories
        , transformFactories = transformFactories
        , filterFactories = filterFactories
        }


test_processTokens_transformFactories : Test
test_processTokens_transformFactories =
    test "test processTokens transformFactories list" <|
        \() ->
            Index.Utils.processTokens
                (createTestIndex1
                    []
                    [ Index.Utils.createFuncCreator (String.dropRight 1), Index.Utils.createFuncCreator (String.dropRight 1) ]
                    []
                )
                [ "awords", "btesting", "ca" ]
                |> Tuple.second
                |> Expect.equal [ "awor", "btesti" ]


test_processTokens_initialTransformFactories : Test
test_processTokens_initialTransformFactories =
    test "test processTokens initialTransformFactories list" <|
        \() ->
            Index.Utils.processTokens
                (createTestIndex1
                    [ Index.Utils.createFuncCreator (String.dropLeft 1), Index.Utils.createFuncCreator (String.dropRight 1) ]
                    []
                    []
                )
                [ "pwords", "qtesting", "ra" ]
                |> Tuple.second
                |> Expect.equal
                    [ "word", "testin" ]


test_processTokens_filterFactories : Test
test_processTokens_filterFactories =
    test "test processTokens filterFactories list" <|
        \() ->
            Index.Utils.processTokens
                (createTestIndex1
                    []
                    []
                    [ createFilterFunc [ "special" ], createFilterFunc [ "swimming" ] ]
                )
                [ "word", "special", "puzzle", "swimming" ]
                |> Tuple.second
                |> Expect.equal
                    [ "word", "puzzle" ]
