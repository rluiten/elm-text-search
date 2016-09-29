module IndexTests exposing (..)

import Dict
import ElmTest exposing (..)
import String

import Index
import Index.Model exposing ( Index(..) )


-- useful with |> thenAnd chaining. avoid infix `Result.andThen`
thenAnd = flip Result.andThen


tests : Test
tests =
    suite "Index tests"
      [ suite "Index search tests" (List.map searchTest searchCases)
      , searchErr1 ()
      , searchErr2 ()
      , searchErr3 ()
      , idfCache1 ()
      , idfCache2 ()
      , addErr1 ()
      , addErr2 ()
      , addErr3 ()
      , removeErr1 ()
      , removeErr2 ()
      , addDocsTest ()
      , searchDocsTest ()
      , searchDocsTestList ()
      , searchDocsTestList2 ()
      , test_index3_add_doc3 ()
      , test_index3_addOrUpdate_doc3 ()
      , test_index2_addOrUpdate_doc3 ()
      ]


-- example record type for tests
type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


type alias MyDoc2 =
    { cid : String
    , title : String
    , author : String
    , body : List String
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


-- example index with indexed List String field
index0list : Index MyDoc2
index0list =
    Index.new
      { indexType = "- IndexTest Type -"
      , ref = .cid
      , fields =
          [ ( .title, 5 )
          ]
      , listFields =
          [ ( .body, 1 )
          ]
      }


type alias IndexResult = Result String (Index MyDoc)
type alias IndexAndListResult = Result String (Index MyDoc, List (String, Float))


{-
These are convenience functions to create tests.
They remove having to deal with Result types.
For use with cases that don't have setups that cause Err Results
-}
safeIndex : (() -> IndexResult) -> Index MyDoc
safeIndex result = Result.withDefault index0 (result ())


safeSearch : (() -> IndexAndListResult) -> (Index MyDoc, List (String, Float))
safeSearch result = Result.withDefault (index0, []) (result ())


safeSearchIndex : (() -> IndexAndListResult) -> Index MyDoc
safeSearchIndex result = fst (safeSearch result)


safeSearchList : (() -> IndexAndListResult) -> List (String, Float)
safeSearchList result = snd (safeSearch result)


index1 : () -> IndexResult
index1 _ = Index.add (doc1 ()) index0


index2 : () -> IndexResult
index2 _ = Index.add (doc2 ()) (safeIndex index1)


index2_banana : () -> IndexAndListResult
index2_banana _ = Index.search "banana"  (safeIndex index2)


index3 : () -> IndexResult
index3 _ = Index.add (doc3 ()) (safeIndex index2)


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
    , title = "Grown Bananas and there appeal"
    , author = "John Banana"
    , body = "An example of apple engineering."
    }


doc3 : () -> MyDoc
doc3 _ =
    { cid = "doc3"
    , title = "Kites and Trees a tail of misery"
    , author = "Adam Winddriven"
    , body = "When a flyer meets an Elm it maybe a problem."
    }


-- document has empty indexing fields
doc4 : () -> MyDoc
doc4 _ =
    { cid = "doc4"
    , title = ""
    , author = "Some Author"
    , body = ""
    }


-- document has empty reference
doc5 : () -> MyDoc
doc5 _ =
    { cid = ""
    , title = "Empty Reference Title"
    , author = "Some Author"
    , body = "Empty Reference Body"
    }


searchCases =
    [ ( "two docs one with term in title first", "example"
      , ["doc1", "doc2"], (safeIndex index2))
    , ( "two docs one with term in title first", "grown"
      , ["doc2", "doc1"], (safeIndex index2))
    , ( "neither document contains both words so return nothing", "-misery! .appeal,"
      , [], (safeIndex index2))
    , ( "with doc3 returns no docs with both words", "-misery! .appeal,"
      , [], (safeIndex index3))
    , ( "returns doc1 and doc2 e expands to example and engineer which exist in both documents."
      , "e"
      , ["doc1","doc2"], (safeIndex index2))
    , ( "search \"ex\" returns doc1, doc2 as both contain example."
      , "ex"
      , ["doc1","doc2"], (safeIndex index2))
    , ( "search \"en\" returns doc2 as it contains engineering."
      , "en"
      , ["doc2"], (safeIndex index2))
    ]


searchTest (name, input, expect, index) =
    test ("search \"" ++ input ++ "\" " ++ name) <|
      assertEqual expect <|
        let
          result = Index.search input index
        in
          case result of
            Ok (index, docs) -> (List.map fst docs)
            Err _ -> ([]) -- Debug.crash(name)


searchErr1 _ =
    test "empty query returns Err" <|
      assertEqual (Err "Error query is empty.") <|
        Index.search "" (safeIndex index2)


searchErr2 _ =
    test "query full of stop words (filtered out words) returns Err" <|
      assertEqual (Err "Error after tokenisation there are no terms to search for.") <|
        Index.search "if and but " (safeIndex index2)


searchErr3 _ =
    test "no document returns Err" <|
      assertEqual (Err "Error there are no documents in index to search.") <|
        Index.search "hello world" index0


idfCache1 _ =
    test "idfCache is cleared after a successful remove document." <|
      assertEqual (Ok "IDF Cache Empty")
        ( (Index.remove (doc1 ()) (safeSearchIndex index2_banana))
          |> thenAnd (\u2index2 -> (idfCacheStateTestMessage u2index2))
        )


idfCache2 _ =
    test "idfCache is cleared after a successful add document." <|
      assertEqual (Ok "IDF Cache Empty")
        ( (Index.add (doc3 ()) (safeSearchIndex index2_banana))
          |> thenAnd (\u2index2 -> (idfCacheStateTestMessage u2index2))
        )


addErr1 _ =
    test "Add a doc with has all index fields empty returns Err" <|
      assertEqual (Err "Error after tokenisation there are no terms to index.") <|
        Index.add (doc4 ()) index0


addErr2 _ =
    test "Add a doc with Err field empty returns Err" <|
      assertEqual (Err "Error document has an empty unique id (ref).") <|
        Index.add (doc5 ()) index0


addErr3 _ =
    test "Add a doc allready in index returns Err" <|
      assertEqual (Err "Error adding document that allready exists.") <|
        Index.add (doc1 ()) (safeIndex index2)


documentStoreStateTestMessage index =
    if (Dict.size index.documentStore) == 0 then
      Ok "Document Store Empty"
    else
      Err "ERROR Document Store is NOT Empty"


idfCacheStateTestMessage (Index irec) =
    if (Dict.size irec.idfCache) == 0 then
      Ok "IDF Cache Empty"
    else
      Err "ERROR IDF Cache Empty is NOT Empty"


removeErr1 _ =
    test "Remove a doc with ref not in index returns Err." <|
      assertEqual (Err "Error document is not in index.") <|
        Index.remove (doc3 ()) (safeIndex index2)


removeErr2 _ =
    test "Remove a doc with Err field empty is an error." <|
      assertEqual (Err "Error document has an empty unique id (ref).") <|
        Index.remove (doc5 ()) (safeIndex index2)


addDocsTest _ =
  suite "addAllDocs Tests" <|
    [ test "Add multiple docs returning list of errors" <|
        assertEqual [(1, "Error after tokenisation there are no terms to index.")] <|
          snd (Index.addDocs [doc3 (), doc4 ()] index0)
    ,  test "Add multiple docs returning list of errors swap order of documents." <|
        assertEqual [(0, "Error after tokenisation there are no terms to index.")] <|
          snd (Index.addDocs [doc4 (), doc3 ()] index0)
    ]


docQ1 : MyDoc
docQ1 =
    { cid = "qdoc1"
    , title = "Question1"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


docQ2 :MyDoc
docQ2 =
    { cid = "qdoc2"
    , title = "Question2"
    , author = "John Banana"
    , body = "An example of apple engineering."
    }


-- Case from https://github.com/rluiten/elm-text-search/issues/4
-- Two docs with titles Question1 and Question2
-- "q" search was not returning both documents.
searchDocsTest _ =
  let
    (index, _) = Index.addDocs [docQ1, docQ2] index0
    searchResult = Index.search "q" index
    collapsedSearchResult =
      case searchResult of
        Ok (index, results) ->
            List.map fst results
        Err msg ->
            []
  in
    test "search String fields results are" <|
        assertEqual
          ["qdoc1", "qdoc2"]
          collapsedSearchResult


docQ1list : MyDoc2
docQ1list =
    { cid = "qdoc1"
    , title = "Question1 Green"
    , author = "Sally Apples"
    , body =
        [ "Sally writes words about "
        , "a grown blue banana."
        ]
    }


docQ2list :MyDoc2
docQ2list =
    { cid = "qdoc2"
    , title = "Question2 Purple"
    , author = "John Banana"
    , body =
      [ "An example of "
      , "green apple engineering."
      ]
    }


-- Configure to have some data in listFields body, match title
searchDocsTestList _ =
  let
    (index, _) = Index.addDocs [docQ1list, docQ2list] index0list
    searchResult = Index.search "q" index
    collapsedSearchResult =
      case searchResult of
        Ok (index, results) ->
            List.map fst results
        Err msg ->
            []
  in
    test "search List String fields where match in title results are" <|
        assertEqual
          ["qdoc1", "qdoc2"]
          collapsedSearchResult


-- Configure to have some data in listFields body, match in listFields body
searchDocsTestList2 _ =
  let
    (index, _) = Index.addDocs [docQ1list, docQ2list] index0list
    searchResult = Index.search "green" index
    collapsedSearchResult =
      case searchResult of
        Ok (index, results) ->
            List.map fst results
        Err msg ->
            []
  in
    test "search List String fields where match in body List String and title" <|
        assertEqual
          ["qdoc1", "qdoc2"]
          collapsedSearchResult


-- verify add of existing document causes an error
test_index3_add_doc3 _ =
  let
    updated = Index.add (doc3 ()) (safeIndex index3)
    indexCreatedOk : Index doc -> Result String String
    indexCreatedOk _ = Ok "Index"
  in
    test "add same document to index produces error" <|
      assertEqual
        (Err "Error adding document that allready exists.")
        (updated `Result.andThen` indexCreatedOk)


test_index3_addOrUpdate_doc3 _ =
  let
    updated = Index.addOrUpdate (doc3 ()) (safeIndex index3)
    indexCreatedOk : Index doc -> Result String String
    indexCreatedOk _ = Ok "Index"
  in
    test "addOrUpdate same document does not produce error" <|
      assertEqual
        (Ok "Index")
        (updated `Result.andThen` indexCreatedOk)


test_index2_addOrUpdate_doc3 _ =
  let
    updated = Index.addOrUpdate (doc3 ()) (safeIndex index2)
    indexCreatedOk : Index doc -> Result String String
    indexCreatedOk _ = Ok "Index"
  in
    test "addOrUpdate document not in index updates index with new doc" <|
      assertEqual
        (Ok "Index")
        (updated `Result.andThen` indexCreatedOk)
