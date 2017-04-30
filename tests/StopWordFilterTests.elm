module StopWordFilterTests exposing (..)

import Expect
import Stemmer
import Test exposing (..)
import ElmTextSearch
import Index.Utils
import StopWordFilter


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


{-| Instantiate stop word filter.
-}
( testIndex1, stopWordFilter ) =
    (StopWordFilter.createDefaultFilterFunc) newIndex


tests : Test
tests =
    describe "StopWordFilter tests"
        [ describe "check stopEnglishWordList against default token processing"
            (List.map stopWordFilterTest StopWordFilter.stopEnglishWordList)
        ]


stopWordFilterTest word =
    test ("This word \"" ++ word ++ "\" got past default stop word filter in error.") <|
        \() ->
            (stopWordFilter word)
                |> Expect.false "These should all be stopped"
