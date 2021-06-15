module IndexTests exposing
    ( addDocAlreadyInIndexReturnsError
    , addDocWithEmptyIdFieldReturnsError
    , addDocWithIndexFieldsEmptyReturnsError
    , addDocumentWithSameIdAsExistingReturnsError
    , addMultipleDocsReturnsErrorListForProblems
    , addOrUpdateDocNotInIndexReturnsSuccess
    , addOrUpdateDocWithSameIdReturnsSuccess
    , idfCacheIsClearedAfterASuccessfulAdd
    , idfCacheIsClearedAfterSuccessfulRemove
    , removeDocRefNotIndexReturnsError
    , removeDocWithEmptyIdFieldReturnsError
    , removeDoesNotBreakSearchResults
    , removeOnlyDocIndexReturnsIsEmpty
    , searchCasesTest
    , searchEmptyIndexReturnsError
    , searchListFieldsSingleLetterWithLetterInBody
    , searchSingleLetterWithLetterInTitles
    , searchUsingEmptyQueryReturnsError
    , searchUsingQueryWithOnlyStopWordsWhichMeansEmptyReturnsError
    , searchWithOnlyListFieldsIndexReturnsValidScores
    , updateDocNotInIndexReturnsError
    , updateDocUsesNewDocContent
    )

import Dict
import ElmTextSearch.Json.Encoder as IndexEncoder
import Expect
import Index
import Index.Model exposing (Index(..))
import Json.Encode as Encode
import Test exposing (..)
import TestUtils
import Trie


{-| example record type for tests
-}
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


doc1_ : MyDoc
doc1_ =
    { cid = "doc1"
    , title = "Examples of a Banana"
    , author = "Sally Apples"
    , body = "Sally writes words about a grown banana."
    }


doc2_ : MyDoc
doc2_ =
    { cid = "doc2"
    , title = "Grown Bananas and there appeal"
    , author = "John Banana"
    , body = "An example of apple engineering."
    }


doc3_ : MyDoc
doc3_ =
    { cid = "doc3"
    , title = "Kites and Trees a tail of misery"
    , author = "Adam Winddriven"
    , body = "When a flyer meets an Elm it maybe a problem."
    }


doc4_indexFieldsEmpty : { cid : String, title : String, author : String, body : String }
doc4_indexFieldsEmpty =
    { cid = "doc4"
    , title = ""
    , author = "Some Author"
    , body = ""
    }


doc5_idEmpty : MyDoc
doc5_idEmpty =
    { cid = ""
    , title = "Empty Reference Title"
    , author = "Some Author"
    , body = "Empty Reference Body"
    }


type alias SearchCaseRecord =
    { name : String
    , input : String
    , expect : List String
    , indexResult : Index MyDoc
    }


searchCasesTest : Test
searchCasesTest =
    describe "Index search tests"
        (List.map searchTestCase
            [ { name = "two docs one with term in title first and body second"
              , input = "example"
              , expect = [ "doc1", "doc2" ]
              , indexResult = getIndexDoc1Doc2 ()
              }
            , { name = "two docs one with term in title first"
              , input = "grown"
              , expect = [ "doc2", "doc1" ]
              , indexResult = getIndexDoc1Doc2 ()
              }
            , { name = "neither document contains both words so return nothing"
              , input = "-misery! .appeal,"
              , expect = []
              , indexResult = getIndexDoc1Doc2 ()
              }
            , { name = "with doc3 returns no docs with both words"
              , input = "-misery! .appeal,"
              , expect = []
              , indexResult = getIndexDoc1Doc2Doc3 ()
              }
            , { name = "returns doc1 and doc2 e expands to example and engineer which exist in both documents."
              , input = "e"
              , expect = [ "doc1", "doc2" ]
              , indexResult = getIndexDoc1Doc2 ()
              }
            , { name = "search \"ex\" returns doc1, doc2 as both contain example."
              , input = "ex"
              , expect = [ "doc1", "doc2" ]
              , indexResult = getIndexDoc1Doc2 ()
              }
            , { name = "search \"en\" returns doc2 as it contains engineering."
              , input = "en"
              , expect = [ "doc2" ]
              , indexResult = getIndexDoc1Doc2 ()
              }
            ]
        )


searchTestCase : SearchCaseRecord -> Test
searchTestCase { name, input, expect, indexResult } =
    test ("search \"" ++ input ++ "\" " ++ name) <|
        \() ->
            Expect.equal expect <|
                case Index.search input indexResult of
                    Ok ( _, docs ) ->
                        List.map Tuple.first docs

                    Err err ->
                        [ err ]


getEmptyIndexMyDoc2IndexOnlyListFields : () -> Index.Index MyDoc2
getEmptyIndexMyDoc2IndexOnlyListFields _ =
    Index.new
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields = []
        , listFields =
            [ ( .body, 1 )
            ]
        }


getEmptyIndexMyDoc2 : () -> Index.Index MyDoc2
getEmptyIndexMyDoc2 _ =
    Index.new
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields = [ ( .title, 5 ) ]
        , listFields = [ ( .body, 1 ) ]
        }


getEmptyIndex : () -> Index.Index MyDoc
getEmptyIndex _ =
    Index.new
        { indexType = "- IndexTest Type -"
        , ref = .cid
        , fields = [ ( .title, 5 ), ( .body, 1 ) ]
        , listFields = []
        }


getIndexDoc1 : () -> Index.Index MyDoc
getIndexDoc1 _ =
    getEmptyIndex ()
        |> Index.add doc1_
        |> TestUtils.getResultIgnoreError


getIndexDoc1Doc2 : () -> Index.Index MyDoc
getIndexDoc1Doc2 _ =
    getIndexDoc1 ()
        |> Index.add doc2_
        |> TestUtils.getResultIgnoreError


getIndexDoc1Doc2Doc3 : () -> Index.Index MyDoc
getIndexDoc1Doc2Doc3 _ =
    getIndexDoc1Doc2 ()
        |> Index.add doc3_
        |> TestUtils.getResultIgnoreError


searchUsingEmptyQueryReturnsError : Test
searchUsingEmptyQueryReturnsError =
    test "empty query returns Err" <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.search ""
                |> Expect.equal (Err "Error query is empty.")


searchUsingQueryWithOnlyStopWordsWhichMeansEmptyReturnsError : Test
searchUsingQueryWithOnlyStopWordsWhichMeansEmptyReturnsError =
    test "query full of stop words (filtered out words) returns Err" <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.search "if and but "
                |> Expect.equal (Err "Error after tokenisation there are no terms to search for.")


searchEmptyIndexReturnsError : Test
searchEmptyIndexReturnsError =
    test "no document returns Err" <|
        \() ->
            Index.search "hello world"
                (getEmptyIndex ())
                |> Expect.equal (Err "Error there are no documents in index to search.")


idfCacheIsClearedAfterSuccessfulRemove : Test
idfCacheIsClearedAfterSuccessfulRemove =
    test "idfCache is cleared after a successful remove document." <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.search "banana"
                |> TestUtils.getResultIgnoreError
                |> Tuple.first
                |> Index.remove doc1_
                |> TestUtils.getResultIgnoreError
                |> getIdfCache
                |> Dict.isEmpty
                |> Expect.true "IdfCache should be cleared after document remove"


idfCacheIsClearedAfterASuccessfulAdd : Test
idfCacheIsClearedAfterASuccessfulAdd =
    test "idfCache is cleared after a successful add document." <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.search "banana"
                |> TestUtils.getResultIgnoreError
                |> Tuple.first
                |> Index.add doc3_
                |> TestUtils.getResultIgnoreError
                |> getIdfCache
                |> Dict.isEmpty
                |> Expect.true "IdfCache should be cleared after document add"


addDocWithIndexFieldsEmptyReturnsError : Test
addDocWithIndexFieldsEmptyReturnsError =
    test "Add a doc which has all index fields empty returns Err" <|
        \() ->
            getEmptyIndex ()
                |> Index.add doc4_indexFieldsEmpty
                |> TestUtils.getErrorIgnoreResult
                |> Expect.equal "Error after tokenisation there are no terms to index."


addDocWithEmptyIdFieldReturnsError : Test
addDocWithEmptyIdFieldReturnsError =
    test "Add a doc empty ID field returns Err" <|
        \() ->
            getEmptyIndex ()
                |> Index.add doc5_idEmpty
                |> Expect.equal (Err "Error document has an empty unique id (ref).")


addDocAlreadyInIndexReturnsError : Test
addDocAlreadyInIndexReturnsError =
    test "Add a doc allready in index returns Err" <|
        \() ->
            getIndexDoc1Doc2Doc3 ()
                |> Index.add doc1_
                |> TestUtils.getErrorIgnoreResult
                |> Expect.equal "Error adding document that allready exists."


getIdfCache : Index doc -> Dict.Dict String Float
getIdfCache (Index irec) =
    irec.idfCache


removeDocRefNotIndexReturnsError : Test
removeDocRefNotIndexReturnsError =
    test "Remove a doc ref not in index returns Err." <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.remove doc3_
                |> TestUtils.getErrorIgnoreResult
                |> Expect.equal "Error document is not in index."


removeDocWithEmptyIdFieldReturnsError : Test
removeDocWithEmptyIdFieldReturnsError =
    test "Remove a doc with empty id field is an error." <|
        \() ->
            getEmptyIndex ()
                |> Index.remove doc5_idEmpty
                |> Expect.equal (Err "Error document has an empty unique id (ref).")


removeDoesNotBreakSearchResults : Test
removeDoesNotBreakSearchResults =
    test "Remove does not break searching" <|
        \() ->
            getIndexDoc1Doc2 ()
                |> Index.remove doc2_
                |> TestUtils.getResultIgnoreError
                |> Index.search "Sally"
                |> TestUtils.getResultIgnoreError
                |> Tuple.second
                |> List.map Tuple.first
                |> Expect.equal [ doc1_.cid ]


{-| Test to verify removing only document reports
-}
removeOnlyDocIndexReturnsIsEmpty : Test
removeOnlyDocIndexReturnsIsEmpty =
    let
        testIndexU1 =
            getIndexDoc1 ()
                |> Index.remove doc1_
                |> TestUtils.getResultIgnoreError

        ( storeB, tokenStoreB ) =
            case testIndexU1 of
                Index { documentStore, tokenStore } ->
                    ( documentStore, tokenStore )
    in
    describe "removing a doc"
        [ test "removes it from document store" <|
            \() ->
                Dict.member "doc1" storeB
                    |> Expect.false "oops its in document store"
        , test "removes trie nodes not leading to a reference. This is not testing trie, testing Index use of trie" <|
            \() ->
                Trie.isEmpty tokenStoreB
                    |> Expect.true "Trie model is not empty"
        ]


addMultipleDocsReturnsErrorListForProblems : Test
addMultipleDocsReturnsErrorListForProblems =
    describe "addAllDocs Tests" <|
        [ test "Add multiple docs returning list of docs with errors" <|
            \() ->
                getEmptyIndex ()
                    |> Index.addDocs [ doc3_, doc4_indexFieldsEmpty ]
                    |> Tuple.second
                    |> Expect.equal [ ( 1, "Error after tokenisation there are no terms to index." ) ]
        , test "Add multiple docs returning list of errors swap order of documents." <|
            \() ->
                getEmptyIndex ()
                    |> Index.addDocs [ doc4_indexFieldsEmpty, doc3_ ]
                    |> Tuple.second
                    |> Expect.equal [ ( 0, "Error after tokenisation there are no terms to index." ) ]
        ]


helperAddDocsSearchIndexResults : String -> List doc -> Index doc -> List ( String, Float )
helperAddDocsSearchIndexResults search docs index =
    index
        -- |> (\a -> Debug.log "foo" a)
        |> Index.addDocs docs
        |> Tuple.first
        |> Index.search search
        |> TestUtils.getResultIgnoreError
        |> Tuple.second


{-| Case from <https://github.com/rluiten/elm-text-search/issues/4>
Two docs with titles Question1 and Question2
"q" search was not returning both documents.
-}
searchSingleLetterWithLetterInTitles : Test
searchSingleLetterWithLetterInTitles =
    test "search single letter reports both documents with word starting with that letter in title field" <|
        \() ->
            getEmptyIndex ()
                |> helperAddDocsSearchIndexResults "q"
                    [ { cid = "qdoc1"
                      , title = "Question1"
                      , author = "Sally Apples"
                      , body = "Sally writes words about a grown banana."
                      }
                    , { cid = "qdoc2"
                      , title = "Question2"
                      , author = "John Banana"
                      , body = "An example of apple engineering."
                      }
                    ]
                |> List.map Tuple.first
                |> Expect.equal [ "qdoc1", "qdoc2" ]


searchListFieldsSingleLetterWithLetterInBody : Test
searchListFieldsSingleLetterWithLetterInBody =
    test "search finds words in list fields body of MyDoc2" <|
        \() ->
            getEmptyIndexMyDoc2 ()
                |> helperAddDocsSearchIndexResults "green"
                    [ { cid = "qdoc1"
                      , title = "Question1 Notgreen"
                      , author = "Sally Apples"
                      , body =
                            [ "Sally writes words about "
                            , "a grown green banana."
                            ]
                      }
                    , { cid = "qdoc2"
                      , title = "Question2 Purple"
                      , author = "John Banana"
                      , body =
                            [ "An example of "
                            , "green apple engineering."
                            ]
                      }
                    ]
                |> List.map Tuple.first
                |> Expect.equal [ "qdoc2", "qdoc1" ]


{-| Configure to have some data in listFields body, match in listFields body, index with fields set to []
Reproduce a bug reported.
-}
searchWithOnlyListFieldsIndexReturnsValidScores : Test
searchWithOnlyListFieldsIndexReturnsValidScores =
    test "search index with only List fields configured, check for NaN values in scores" <|
        \() ->
            getEmptyIndexMyDoc2IndexOnlyListFields ()
                |> helperAddDocsSearchIndexResults "green"
                    [ { cid = "qdoc1"
                      , title = "Question1 Notgreen"
                      , author = "Sally Apples"
                      , body =
                            [ "Sally writes words about "
                            , "a grown green banana."
                            ]
                      }
                    , { cid = "qdoc2"
                      , title = "Question2 Purple"
                      , author = "John Banana"
                      , body =
                            [ "An example of "
                            , "green apple engineering."
                            ]
                      }
                    ]
                |> List.map Tuple.second
                |> List.any Basics.isNaN
                |> Expect.false "Expect searchScores to not contain any NaN values"


addDocumentWithSameIdAsExistingReturnsError : Test
addDocumentWithSameIdAsExistingReturnsError =
    test "add same document to index produces error" <|
        \() ->
            getIndexDoc1 ()
                |> Index.add doc1_
                |> TestUtils.getErrorIgnoreResult
                |> Expect.equal "Error adding document that allready exists."


addOrUpdateDocWithSameIdReturnsSuccess : Test
addOrUpdateDocWithSameIdReturnsSuccess =
    test "addOrUpdate same document does not produce error" <|
        \() ->
            getIndexDoc1 ()
                |> Index.addOrUpdate doc1_
                |> TestUtils.isOk
                |> Expect.true "Expect Ok result to addOrUpdate if doc in index"


addOrUpdateDocNotInIndexReturnsSuccess : Test
addOrUpdateDocNotInIndexReturnsSuccess =
    test "addOrUpdate document not in index updates index with new doc" <|
        \() ->
            getEmptyIndex ()
                |> Index.addOrUpdate doc1_
                |> TestUtils.isOk
                |> Expect.true "Expect Ok result to addOrUpdate if doc is new"


updateDocNotInIndexReturnsError : Test
updateDocNotInIndexReturnsError =
    test "index update with a doc not in index fails" <|
        \() ->
            getEmptyIndex ()
                |> Index.update doc1_
                |> TestUtils.isOk
                |> Expect.false "Updating a doc not in index fails"


{-| Updating a document removes old doc version and adds new doc version.

This was a bug I noticed in code, writing test to confirm before fixing it.

-}
updateDocUsesNewDocContent : Test
updateDocUsesNewDocContent =
    let
        indexT1 =
            getEmptyIndex ()
                |> Index.addDocs
                    [ { cid = "qdoc1"
                      , title = "Question1"
                      , author = "Sally Apples"
                      , body = "Sally writes words about a grown banana."
                      }
                    , { cid = "qdoc2"
                      , title = "Question2"
                      , author = "John Banana"
                      , body = "An example of apple engineering."
                      }
                    ]
                |> Tuple.first

        indexT2 =
            indexT1
                |> Index.update
                    { cid = "qdoc1"
                    , title = "Yesterday"
                    , author = "New User"
                    , body = "Completely different document really"
                    }
                |> TestUtils.getResultIgnoreError

        encodedT1 =
            indexT1
                |> IndexEncoder.encoder
                |> Encode.encode 0

        encodedT2 =
            indexT2
                |> IndexEncoder.encoder
                |> Encode.encode 0
    in
    test "updateDoc removes old doc and replaces it so index changes" <|
        \() ->
            encodedT1
                |> Expect.notEqual
                    encodedT2
