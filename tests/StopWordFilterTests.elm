module StopWordFilterTests where

import ElmTest exposing (..)

import ElmTextSearch
import Index.Utils
import StopWordFilter
import Stemmer


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
    }


-- instantiate stop word filter
(testIndex1, stopWordFilter) = (StopWordFilter.createDefaultFilterFunc) newIndex
-- get the default stop word list and run through default index transform first
(testIndex2, testWordList) = Index.Utils.applyTransform testIndex1 StopWordFilter.stopEnglishWordList


tests : Test
tests =
    suite "StopWordFilter tests"
      [ suite "check stopEnglishWordList agains default token processing"
          (List.map stopWordFilterTest testWordList)
      ]


stopWordFilterTest word =
    test ("This word \"" ++ word ++ "\" got past default stop word filter in errror.") <|
      assert (not (stopWordFilter word))
