module StopWordFilterTests exposing (newIndex, stopWordFilterTest, tests)

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


newIndex : ElmTextSearch.Index ExampleDocType
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
    describe "check stopEnglishWordList against default token processing"
        (List.map stopWordFilterTest StopWordFilter.stopEnglishWordList)


stopWordFilterTest : String -> Test
stopWordFilterTest word =
    let
        ( _, stopWordFilter ) =
            StopWordFilter.createDefaultFilterFunc newIndex
    in
    test ("This word \"" ++ word ++ "\" got past default stop word filter in error.") <|
        \() ->
            stopWordFilter word
                |> Expect.equal False
                >> Expect.onFail "These should all be stopped"
