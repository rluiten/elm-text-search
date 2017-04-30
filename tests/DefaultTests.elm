module DefaultTests exposing (..)

import Index.Defaults
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Default tests"
        [ testDefaultIndexType ()
        ]


testDefaultIndexType _ =
    let
        configDefault =
            Index.Defaults.getIndexSimpleConfig
                { ref = .cid
                , fields =
                    [ ( .title, 5.0 )
                    ]
                , listFields =
                    [ ( .body, 1.0 )
                    ]
                }

        _ =
            Debug.log "asdfasdfafsd" configDefault
    in
        test "Check Index Type" <|
            \() ->
                Expect.equal "-= ElmTextSearch Index Type 1 =-" configDefault.indexType



-- testDefaultIndexVersion _ =
--     test "Check Index Version" <|
--         \() ->
--             Expect.equal "-= ElmTextSearch Index Type 2 =-" configDefault.indexType
