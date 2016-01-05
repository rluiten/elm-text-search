module SaveLoadTests where

import ElmTest exposing (..)

{- Save and Load index check search results same -}
import ElmTextSearch
import Index.Model exposing ( Index(..) )
import TestUtils exposing (assertOk, assertErr)


-- useful with |> thenAnd chaining. avoid infix `Result.andThen`
thenAnd = flip Result.andThen


tests : Test
tests =
    suite "Save and Load index tests"
      [ checkSearchResult1()
      , checkSearchResult2()
      , saveAndLoadSame ()
      ]


-- type alias IndexResult =
--     Result String (Index MyDoc)
-- type alias IndexAndListResult =
--     Result String (Index MyDoc, List (String, Float))


-- example record type for tests
type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }

configElmTextSearchMyDoc =
    { ref = .cid
    , fields =
        [ ( .title, 5 )
        , ( .body, 1 )
        ]
    }

doc1 : () -> MyDoc
doc1 _ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


doc2 : () -> MyDoc
doc2 _ =
    { cid = "doc2"
    , title = "Words about a vehicle"
    , author = "John Barrel"
    , body = "All about a vehicle in exile."
    }


-- example index
index0 : Index MyDoc
index0 =
    ElmTextSearch.new configElmTextSearchMyDoc


index1 : Result String (Index MyDoc)
index1 = ElmTextSearch.add (doc1 ()) index0


index2 : Result String (Index MyDoc)
index2 =
    index1
      |> thenAnd (ElmTextSearch.add (doc2 ()))


result1Search : Result String (Index MyDoc, List (String, Float))
result1Search =
    index2
    |> thenAnd (ElmTextSearch.search "foreign")


result2Search : Result String (Index MyDoc, List (String, Float))
result2Search =
    index2
    |> thenAnd (ElmTextSearch.search "exile")


result1StoreToString : Result String String
result1StoreToString =
    result1Search
    |> thenAnd
        ( \searchResult ->
            Ok (ElmTextSearch.storeToString (fst searchResult))
        )


result1FromString : Result String (Index MyDoc)
result1FromString =
    result1StoreToString
      |> thenAnd (ElmTextSearch.fromString configElmTextSearchMyDoc)


result1AfterLoadSearch : Result String (Index MyDoc, List (String, Float))
result1AfterLoadSearch =
    result1FromString
    |> thenAnd (ElmTextSearch.search "foreign")


result2AfterLoadSearch : Result String (Index MyDoc, List (String, Float))
result2AfterLoadSearch =
    result1FromString
    |> thenAnd (ElmTextSearch.search "exile")


checkSearchResult1 _ =
    let
      result1 : Result String (List (String, Float))
      result1 =
        result1Search
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      -- _ = Debug.log("checkSearchResult1") (result1)
    in
      test "Search returns empty result." <|
        assertEqual (Ok []) result1


checkSearchResult2 _ =
    let
      result2 : Result String (List (String, Float))
      result2 =
        result2Search
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      -- _ = Debug.log("checkSearchResult2") (result2Search)
    in
      test "Search returns empty result." <|
        assertEqual (Ok [("doc2",0.1389834449709609)]) result2


saveAndLoadSame _ =
    let
      result1 : Result String (List (String, Float))
      result1 =
        result1Search
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      result1AfterLoad : Result String (List (String, Float))
      result1AfterLoad =
        result1AfterLoadSearch
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      -- _ = Debug.log("saveAndLoadSame") (result1, result1AfterLoad)

      result2 : Result String (List (String, Float))
      result2 =
        result2Search
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      result2AfterLoad : Result String (List (String, Float))
      result2AfterLoad =
        result2AfterLoadSearch
        |> thenAnd (\searchResult -> Ok (snd searchResult))
      _ = Debug.log("saveAndLoadSame") (result2, result2AfterLoad)

    in
      suite "results same before and after save and load index"
        [ test "Search result of nothing for Index same as for Save and Loaded Index." <|
            assertEqual result1 result1AfterLoad
        , test "Search result of something for Index same as for Save and Loaded Index." <|
            assertEqual result2 result2AfterLoad
        ]
