module DefaultTests exposing (testDefaultIndexType)

import Expect
import Index.Defaults
import Test exposing (..)


testDefaultIndexType : Test
testDefaultIndexType =
    test "Check Index Type" <|
        \() ->
            Index.Defaults.getIndexSimpleConfig
                { ref = .cid
                , fields =
                    [ ( .title, 5.0 )
                    ]
                , listFields =
                    [ ( .body, 1.0 )
                    ]
                }
                |> .indexType
                |> Expect.equal "-= ElmTextSearch Index Type 1 =-"
