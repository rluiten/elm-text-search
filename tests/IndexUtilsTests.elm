module IndexUtilsTests exposing (..)

-- import Dict
import ElmTest exposing (..)
-- import String

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
      , listFields = []
      }


tests : Test
tests =
    suite "Index.Utils tests"
      [ suite "apply default transform tests"
          (List.map testDefaultTransforms defaultTransformCases)
      ]


defaultTransformCases =
    [ ( "words of only non word chars removed"
      , "engineering ???"
      , ["engin"] )
    , ( "stemmer and non word chars removed"
      , ".This was very large.-"
      , ["veri", "larg"] )
    , ( "stop words removed"
      , "however among the dear .- -"
      , [] )
    ]


testDefaultTransforms (name, input, expected) =
  let
    a = 1
  in
    test ("getTokens \"" ++ input ++ "\" " ++ name)
      <| assertEqual
          expected
          (snd (Index.Utils.getTokens index0 input))
