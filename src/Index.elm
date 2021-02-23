module Index exposing
    ( new
    , newWith
    , add
    , addDocs
    , remove
    , update
    , addOrUpdate
    , search
    , Index
    )

{-| Index module for full text indexer


## Create Index

@docs new
@docs newWith


## Update Index

@docs add
@docs addDocs
@docs remove
@docs update
@docs addOrUpdate


## Query Index

@docs search


## Types

@docs Index

Copyright (c) 2016 Robin Luiten

-}

import Dict
import Index.Defaults as Defaults
import Index.Model as Model exposing (Index(..))
import Index.Utils
import Index.Vector exposing (..)
import Maybe
import Set exposing (Set)
import String
import Trie
import Utils


type alias Index doc =
    Model.Index doc


type alias Config doc =
    Model.Config doc


type alias SimpleConfig doc =
    Model.ModelSimpleConfig doc


{-| Create new index.
-}
new : SimpleConfig doc -> Index doc
new simpleConfig =
    newWith
        (Defaults.getDefaultIndexConfig simpleConfig)


{-| Create new index with control of transformers and filters.
-}
newWith : Config doc -> Index doc
newWith { indexType, ref, fields, listFields, initialTransformFactories, transformFactories, filterFactories } =
    Index
        { indexVersion = Defaults.indexVersion
        , indexType = indexType
        , ref = ref
        , fields = fields
        , listFields = listFields
        , initialTransformFactories = initialTransformFactories
        , transformFactories = transformFactories
        , filterFactories = filterFactories
        , initialTransforms = Nothing
        , transforms = Nothing
        , filters = Nothing
        , corpusTokens = Set.empty
        , corpusTokensIndex = Dict.empty
        , documentStore = Dict.empty
        , tokenStore = Trie.empty
        , idfCache = Dict.empty
        }


{-| Add document to an Index if no error conditions found.
See ElmTextSearch documentation for `add` to see error conditions.
-}
add : doc -> Index doc -> Result String (Index doc)
add doc ((Index irec) as index) =
    let
        docRef =
            irec.ref doc
    in
    if String.isEmpty docRef then
        Err "Error document has an empty unique id (ref)."

    else if Index.Utils.refExists docRef index then
        Err "Error adding document that allready exists."

    else
        let
            ( u1index, fieldsWordListAndBoost ) =
                List.foldr
                    (getWordsForField doc)
                    ( index, [] )
                    irec.fields

            ( u2index, u2fieldsWordListAndBoost ) =
                List.foldr
                    (getWordsForFieldList doc)
                    ( u1index, fieldsWordListAndBoost )
                    irec.listFields

            docTokens =
                List.map Tuple.first u2fieldsWordListAndBoost
                    |> List.foldr Set.union Set.empty
        in
        if Set.isEmpty docTokens then
            Err "Error after tokenisation there are no terms to index."

        else
            Ok (addDoc docRef u2fieldsWordListAndBoost docTokens u2index)


{-| Add multiple documents. Tries to add all docs and collects errors..
It does not stop adding at first error encountered.

The result part List (Int, String) is the list of document index
and the error string message result of adding.
Returns the index unchanged if all documents error when added.
Returns the updated index after adding the documents.

-}
addDocs : List doc -> Index doc -> ( Index doc, List ( Int, String ) )
addDocs docs index =
    addDocsCore 0 docs index []


addDocsCore :
    Int
    -> List doc
    -> Index doc
    -> List ( Int, String )
    -> ( Index doc, List ( Int, String ) )
addDocsCore docsI docs index errors =
    case docs of
        [] ->
            ( index, errors )

        headDoc :: tailDocs ->
            case add headDoc index of
                Ok u1index ->
                    addDocsCore (docsI + 1) tailDocs u1index errors

                Err msg ->
                    addDocsCore (docsI + 1) tailDocs index (errors ++ [ ( docsI, msg ) ])


{-| Reducer to extract tokens from each field String from doc.
-}
getWordsForField :
    doc
    -> ( doc -> String, Float )
    -> ( Index doc, List ( Set String, Float ) )
    -> ( Index doc, List ( Set String, Float ) )
getWordsForField doc ( getField, fieldBoost ) ( index, fieldsLists ) =
    -- GRR fieldBoost goes where? dammmit. it doesnt belong here :( its not part of aggregate
    let
        ( u1index, tokens ) =
            Index.Utils.getTokens index (getField doc)
    in
    ( u1index, ( Set.fromList tokens, fieldBoost ) :: fieldsLists )


{-| Reducer to extract tokens from each field List String from doc.
-}
getWordsForFieldList :
    doc
    -> ( doc -> List String, Float )
    -> ( Index doc, List ( Set String, Float ) )
    -> ( Index doc, List ( Set String, Float ) )
getWordsForFieldList doc ( getFieldList, fieldBoost ) ( index, fieldsLists ) =
    let
        ( u1index, tokens ) =
            Index.Utils.getTokensList index (getFieldList doc)
    in
    ( u1index, ( Set.fromList tokens, fieldBoost ) :: fieldsLists )


{-| Add the document to the index.
-}
addDoc : String -> List ( Set String, Float ) -> Set String -> Index doc -> Index doc
addDoc docRef fieldTokensAndBoosts docTokens (Index irec) =
    let
        addTokenScore ( token, score ) trie =
            Trie.add ( docRef, score ) token trie

        updatedDocumentStore =
            Dict.insert docRef docTokens irec.documentStore

        updatedCorpusTokens =
            Set.union irec.corpusTokens docTokens

        -- can the cost of this be reduced ?
        updatedCorpusTokensIndex =
            Index.Utils.buildOrderIndex updatedCorpusTokens

        tokenAndScores =
            List.map
                (scoreToken fieldTokensAndBoosts)
                (Set.toList docTokens)

        updatedTokenStore =
            List.foldr addTokenScore irec.tokenStore tokenAndScores
    in
    Index
        { irec
            | documentStore = updatedDocumentStore
            , corpusTokens = updatedCorpusTokens
            , corpusTokensIndex = updatedCorpusTokensIndex
            , tokenStore = updatedTokenStore
            , idfCache = Dict.empty
        }


{-| Return term frequency score for a token in document.

Overall score for a token is based on the number of fields the word
appears and weighted by boost score on each field.

-}
scoreToken : List ( Set String, Float ) -> String -> ( String, Float )
scoreToken fieldTokensAndBoost token =
    let
        score : ( Set String, Float ) -> Float -> Float
        score ( tokenSet, fieldBoost ) scoreSum =
            if Set.isEmpty tokenSet then
                scoreSum

            else
                let
                    tokenBoost =
                        if Set.member token tokenSet then
                            fieldBoost / toFloat (Set.size tokenSet)

                        else
                            0
                in
                scoreSum + tokenBoost
    in
    ( token, List.foldr score 0 fieldTokensAndBoost )


{-| Remove document from an Index if no error result conditions encountered.

See ElmTextSearch documentation for `remove` to see error result conditions.

This does the following things

  - Remove the document tags from documentStore.
  - Remove all the document references in tokenStore.
  - It does not modify corpusTokens - as this requires
    reprocessing tokens for all documents to recreate corpusTokens.
      - This may skew the results over time after many removes but not badly.
      - It appears lunr.js operates this way as well for remove.

-}
remove : doc -> Index doc -> Result String (Index doc)
remove doc ((Index irec) as index) =
    let
        docRef =
            irec.ref doc

        -- can error without docid as well.
    in
    if String.isEmpty docRef then
        Err "Error document has an empty unique id (ref)."

    else if not (Index.Utils.refExists docRef index) then
        Err errorMessageNotIndex

    else
        Ok
            (Maybe.withDefault index <|
                Maybe.map
                    (removeDoc docRef index)
                    (Dict.get docRef irec.documentStore)
            )


errorMessageNotIndex : String
errorMessageNotIndex =
    "Error document is not in index."


{-| Remove the doc by docRef id from the index.
-}
removeDoc : String -> Index doc -> Set String -> Index doc
removeDoc docRef (Index irec) docTokens =
    let
        removeToken token trie =
            Trie.remove token docRef trie

        updatedDocumentStore =
            Dict.remove docRef irec.documentStore

        updatedTokenStore =
            List.foldr removeToken irec.tokenStore (Set.toList docTokens)
    in
    Index
        { irec
            | documentStore = updatedDocumentStore
            , tokenStore = updatedTokenStore
            , idfCache = Dict.empty
        }


{-| Update document in Index. Does a remove then add.
See ElmTextSearch documentation for `add` and `remove` to see error result conditions.
-}
update : doc -> Index doc -> Result String (Index doc)
update doc index =
    remove doc index
        |> Result.andThen (add doc)


{-| Add or Update document in Index.
This does an add if document is not in index.
-}
addOrUpdate : doc -> Index doc -> Result String (Index doc)
addOrUpdate doc index =
    case remove doc index of
        Ok u1index ->
            add doc u1index

        Err msg ->
            if msg == errorMessageNotIndex then
                add doc index

            else
                Err msg


{-| Search index with query.
See ElmTextSearch documentation for `search` to see error result conditions.
-}
search : String -> Index doc -> Result String ( Index doc, List ( String, Float ) )
search query index =
    let
        ( (Index i1irec) as i1index, tokens ) =
            Index.Utils.getTokens index query

        tokenInStore token =
            Trie.getNode token i1irec.tokenStore /= Nothing

        -- _ = Debug.log "search d" (query, tokens, List.any tokenInStore tokens)
    in
    if Dict.isEmpty i1irec.documentStore then
        Err "Error there are no documents in index to search."

    else if String.isEmpty (String.trim query) then
        Err "Error query is empty."

    else if List.isEmpty tokens then
        Err "Error after tokenisation there are no terms to search for."

    else if List.isEmpty tokens || not (List.any tokenInStore tokens) then
        Ok ( i1index, [] )

    else
        Ok (searchTokens tokens i1index)


{-| Return list of document ref's with score, ordered by score descending.
-}
searchTokens :
    List String
    -> Index doc
    -> ( Index doc, List ( String, Float ) )
searchTokens tokens ((Index irec) as index) =
    let
        fieldBoosts =
            List.sum (List.map Tuple.second irec.fields)

        -- _ = Debug.log("searchTokens") (tokens, fieldBoosts)
        ( tokenDocSets, queryVector, u1index ) =
            Index.Vector.getQueryVector
                fieldBoosts
                tokens
                index

        ( u2index, matchedDocs ) =
            List.foldr
                (scoreAndCompare queryVector)
                ( u1index, [] )
                (Set.toList (Utils.intersectSets tokenDocSets))

        -- _ = Debug.log("searchTokens intersect") (Utils.intersectSets tokenDocSets)
    in
    ( u2index, List.reverse (List.sortBy Tuple.second matchedDocs) )
