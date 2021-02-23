module ElmTextSearchTests exposing (..)

import ElmTextSearch
import ElmTextSearchErrors exposing (AddError(..), RemoveError(..), SearchError(..))
import Expect
import Test exposing (..)


type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


doc1_ : MyDoc
doc1_ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


getEmptyIndex : () -> ElmTextSearch.Index MyDoc
getEmptyIndex _ =
    ElmTextSearch.new
        { ref = .cid
        , fields = [ ( .title, 5 ), ( .body, 1 ) ]
        , listFields = []
        }


test_searchT_CanUseErrorResultConstructors : Test
test_searchT_CanUseErrorResultConstructors =
    test "If can case on error result" <|
        \() ->
            getEmptyIndex ()
                |> ElmTextSearch.searchT "hello"
                |> (\result ->
                        case result of
                            Ok _ ->
                                False

                            Err error ->
                                case error of
                                    IndexIsEmpty ->
                                        True

                                    _ ->
                                        False
                   )
                |> Expect.true "Result should be an error"
