module Index
    ( Index
    , new
    , newWith
    , add
    , remove
    , update
    , search
    ) where

{-| Index module for full text indexer

## Create Index
@docs new
@docs newWith

## Update Index
@docs add
@docs remove
@docs update

## Query Index
@docs search

## Types
@docs Index

Copyright (c) 2016 Robin Luiten
-}

import Maybe exposing (andThen, withDefault)
import Dict exposing (Dict)
import Set exposing (Set)
import String
import Trie exposing (Trie)

import IndexDefaults
import IndexModel exposing (Index (..))
import IndexUtils
import IndexVector exposing (..)
import Utils


type alias Index doc = IndexModel.Index doc
type alias Config doc = IndexModel.Config doc
type alias SimpleConfig doc = IndexModel.SimpleConfig doc


{-| Create new index.
-}
new : SimpleConfig doc -> Index doc
new simpleConfig  =
    newWith
      (IndexDefaults.getDefaultIndexConfig simpleConfig)


{-| Create new index with control of transformers and filters.
-}
newWith : Config doc -> Index doc
newWith {indexType, ref, fields, transformFactories, filterFactories} =
    Index
      { indexVersion = IndexDefaults.indexVersion
      , indexType = indexType
      , ref = ref
      , fields = fields
      , transformFactories = transformFactories
      , filterFactories = filterFactories

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
add : Index doc -> doc -> Result String (Index doc)
add (Index irec as index) doc =
    let
      docRef = irec.ref doc
    in
      if String.isEmpty docRef then
        Err "Error document has an empty unique id (ref)."
      else if IndexUtils.refExists docRef index then
        Err "Error adding document that allready exists."
      else
        let
          (u3index, fieldsWordList) =
            List.foldr
              (getWordsForField doc)
              (index, [])
              (List.map fst irec.fields)
          fieldsTokens = List.map Set.fromList fieldsWordList
          docTokens = List.foldr Set.union Set.empty fieldsTokens
          -- _ = Debug.log("add docTokens") (docTokens)
        in
          if Set.isEmpty docTokens then
            Err "Error after tokenisation there are no terms to index."
          else
            Ok (addDoc docRef u3index fieldsTokens docTokens)


{- reducer to extract tokens from each field of doc -}
getWordsForField :
       doc
    -> (doc -> String)
    -> (Index doc, List (List String))
    -> (Index doc, List (List String))
getWordsForField doc getField (index, fieldsLists) =
    let
      (u1index, tokens) = IndexUtils.getTokens index (getField doc)
    in
      (u1index, tokens :: fieldsLists)


{- Add the document to the index. -}
addDoc : String -> Index doc -> List (Set String) -> Set String -> Index doc
addDoc docRef (Index irec as index) fieldsTokens docTokens =
    let
      addTokenScore (token, score) trie =
        Trie.add (docRef, score) token trie

      fieldsBoosts = List.map snd irec.fields
      -- fieldTokensAndBoosts : List (Set String, Float)
      fieldTokensAndBoosts = List.map2 (,) fieldsTokens fieldsBoosts

      -- updatedDocumentStore : Dict String (Set String)
      updatedDocumentStore = Dict.insert docRef docTokens irec.documentStore
      updatedCorpusTokens = Set.union irec.corpusTokens docTokens
      -- can the cost of this be reduced ?
      updatedCorpusTokensIndex = IndexUtils.buildOrderIndex updatedCorpusTokens
      score = scoreToken fieldTokensAndBoosts
      -- tokenAndScores : List (String, Float)
      tokenAndScores = List.map score (Set.toList docTokens)
      updatedTokenStore = List.foldr addTokenScore irec.tokenStore tokenAndScores
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
scoreToken : List (Set String, Float) -> String -> (String, Float)
scoreToken fieldTokensAndBoost token =
    let
      score : (Set String, Float) -> Float -> Float
      score (tokenSet, fieldBoost) scoreSum =
        if Set.isEmpty tokenSet then
          scoreSum
        else
          let
            tokenBoost =
              if Set.member token tokenSet then
                fieldBoost / (toFloat (Set.size tokenSet))
              else
                0
          in
            scoreSum + tokenBoost
    in
      (token, List.foldr score 0 fieldTokensAndBoost)


{-| Remove document from an Index if no error result conditions encountered.

See ElmTextSearch documentation for `remove` to see error result conditions.

This does the following things
* Remove the document tags from documentStore.
* Remove all the document references in tokenStore.
* It does not modify corpusTokens - as this would required
reprocessing tokens for all documents to recreate corpusTokens.
 * This may skew the results over time after many removes but not badly.
 * It appears lunr.js operates this way as well for remove.
-}
remove : Index doc -> doc -> Result String (Index doc)
remove (Index irec as index) doc =
    let
      docRef = irec.ref doc -- can error without docid as well.
    in -- TODO return Err if doc not in index.
      if String.isEmpty docRef then
        Err "Error document has an empty unique id (ref)."
      else if not (IndexUtils.refExists docRef index) then
        Err "Error document is not in index."
      else
        Ok (
          withDefault index <|
            Maybe.map
              (removeDoc docRef index)
              (Dict.get docRef irec.documentStore)
        )


{- Remove the doc by docRef id from the index. -}
removeDoc : String -> Index doc -> Set String -> Index doc
removeDoc docRef (Index irec as index) docTokens =
    let
      removeToken token trie = Trie.remove token docRef trie
      updatedDocumentStore = Dict.remove docRef irec.documentStore
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
update : Index doc -> doc -> Result String (Index doc)
update index doc =
    (remove index doc)
      `Result.andThen`
      (\u1index -> add index doc)


{-| Search index with query.

See ElmTextSearch documentation for `search` to see error result conditions.
-}
search : Index doc -> String -> Result String (Index doc, List (String, Float))
search index query =
    let
      (Index i1irec as i1index, tokens) = IndexUtils.getTokens index query
      hasToken token = Trie.has token i1irec.tokenStore
      -- _ = Debug.log("search") (query, tokens, List.any hasToken tokens)
    in
      if Dict.isEmpty i1irec.documentStore then
        Err "Error there are no documents in index to search."
      else if String.isEmpty (String.trim query) then
        Err "Error query is empty."
      else if List.isEmpty tokens then
        Err "Error after tokenisation there are no terms to search for."
      else if List.isEmpty tokens || not (List.any hasToken tokens) then
        Ok (i1index, [])
      else
        Ok (searchTokens i1index tokens)


{- Return list of document ref's with score, ordered by score descending. -}
searchTokens :
       Index doc
    -> List String
    -> (Index doc, List (String, Float))
searchTokens (Index irec as index) tokens =
    let
      fieldBoosts = List.sum (List.map snd irec.fields)
      -- _ = Debug.log("searchTokens") (tokens, fieldBoosts)
      (tokenDocSets, queryVector, u1index) =
        IndexVector.getQueryVector
          fieldBoosts
          tokens
          index
      (u2index, matchedDocs) =
        List.foldr
          (scoreAndCompare queryVector)
          (u1index, [])
          (Set.toList (Utils.intersectSets tokenDocSets))
      -- _ = Debug.log("searchTokens intersect") (Utils.intersectSets tokenDocSets)
    in
      (u2index, List.reverse (List.sortBy snd matchedDocs))
