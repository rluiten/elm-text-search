module SearchIndexTests exposing (saveAndLoadSameTest, searchReturnsEmptyResult, searchReturnsValidResult)

{- Save and Load index check search results same -}

import ElmTextSearch
import Expect
import Index.Model exposing (Index(..), IndexSimpleConfig)
import Test exposing (..)
import TestUtils


type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


configElmTextSearchMyDoc : IndexSimpleConfig MyDoc
configElmTextSearchMyDoc =
    { ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    , listFields = []
    }


doc1 : MyDoc
doc1 =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


doc2 : MyDoc
doc2 =
    { cid = "doc2"
    , title = "Words about a vehicle"
    , author = "John Barrel"
    , body = "All about a vehicle in exile."
    }


{-| example index
-}
getEmptyIndex : () -> Index MyDoc
getEmptyIndex _ =
    ElmTextSearch.new configElmTextSearchMyDoc


getIndexDoc1 : () -> Index MyDoc
getIndexDoc1 _ =
    getEmptyIndex ()
        |> ElmTextSearch.add doc1
        |> TestUtils.getResultIgnoreError


getIndexDoc1Doc2 : () -> Index MyDoc
getIndexDoc1Doc2 _ =
    getIndexDoc1 ()
        |> ElmTextSearch.add doc2
        |> TestUtils.getResultIgnoreError


searchReturnsEmptyResult : Test
searchReturnsEmptyResult =
    test "Search returns empty result" <|
        \() ->
            getIndexDoc1Doc2 ()
                |> ElmTextSearch.search "foreign"
                |> TestUtils.getResultIgnoreError
                |> Tuple.second
                |> Expect.equal []


searchReturnsValidResult : Test
searchReturnsValidResult =
    test "Search returns valid result" <|
        \() ->
            getIndexDoc1Doc2 ()
                |> ElmTextSearch.search "exile"
                |> TestUtils.getResultIgnoreError
                |> Tuple.second
                |> Expect.equal [ ( "doc2", 0.13898344497096093 ) ]


{-| helper to save and load an index. and run a search in original index and loaded index.
-}
searchIndexSearchSavedLoadedIndex : String -> Index MyDoc -> ( List ( String, Float ), List ( String, Float ) )
searchIndexSearchSavedLoadedIndex search index =
    let
        searchAnIndex index2 =
            index2
                |> ElmTextSearch.search search
                |> TestUtils.getResultIgnoreError
                |> Tuple.second

        savedAndLoadedIndex i =
            ElmTextSearch.storeToString i
                |> ElmTextSearch.fromString configElmTextSearchMyDoc
                |> TestUtils.getResultIgnoreError
    in
    ( searchAnIndex index, searchAnIndex <| savedAndLoadedIndex index )


saveAndLoadSameTest : Test
saveAndLoadSameTest =
    describe "results same before and after save and load index"
        [ test "x Search result of nothing for Index same as for Save and Loaded Index." <|
            \() ->
                let
                    ( resultA, resultsB ) =
                        getIndexDoc1Doc2 ()
                            |> searchIndexSearchSavedLoadedIndex "foreign"
                in
                Expect.equal resultA resultsB
        , test "x Search result of something for Index same as for Save and Loaded Index." <|
            \() ->
                let
                    ( resultA, resultsB ) =
                        getIndexDoc1Doc2 ()
                            |> searchIndexSearchSavedLoadedIndex "exile"
                in
                Expect.equal resultA resultsB
        ]
