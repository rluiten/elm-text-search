module StopWordFilterTests exposing (ExampleDocType, newIndex, stopWordFilterTest, tests)

import ElmTextSearch
import Expect
import StopWordFilter
import Test exposing (..)


type alias ExampleDocType =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


newIndex =
    ElmTextSearch.new
        { ref = .cid
        , fields =
            [ ( .title, 5 )
            , ( .body, 1 )
            ]
        , listFields = []
        }


tests : Test
tests =
    describe "StopWordFilter tests"
        [ describe "check stopEnglishWordList against default token processing"
            (List.map stopWordFilterTest StopWordFilter.stopEnglishWordList)
        ]


stopWordFilterTest word =
    let
        ( _, stopWordFilter ) =
            StopWordFilter.createDefaultFilterFunc newIndex
    in
    test ("This word \"" ++ word ++ "\" got past default stop word filter in error.") <|
        \() ->
            stopWordFilter word
                |> Expect.false "These should all be stopped"
