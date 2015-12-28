module IndexVector where

{-| Index document vector support. -}

import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)
import Set exposing (Set)
import SparseVector exposing (SparseVector)
import String
import Trie exposing (Trie)

import IndexModel exposing (Index (..))
import IndexUtils


{-| Build a query vector. -}
getQueryVector :
       Float
    -> List String
    -> (Index doc)
    -> (List (Set String) ,SparseVector , Index doc)
getQueryVector fieldBoosts tokens index =
    List.foldr
      (buildDocVector (List.length tokens) fieldBoosts)
      ([], SparseVector.empty, index)
      tokens


{- Update vector of token scores for document. -}
buildDocVector :
       Int
    -> Float
    -> String
    -> (List (Set String), SparseVector, Index doc)
    -> (List (Set String), SparseVector, Index doc)
buildDocVector tokensLength fieldBoosts baseToken (docSets, vec, Index irec as index) =
    let
      termFrequency
        = 1 / (toFloat tokensLength)
        * (toFloat (List.length irec.fields))
        * fieldBoosts
      expandedTokens = Trie.expand baseToken irec.tokenStore
      -- _ = Debug.log("buildDocVector") (tokensLength, baseToken, expandedTokens)
    in
      List.foldr
        (updateSetAndVec termFrequency baseToken)
        (docSets, vec, index)
        expandedTokens


{- Calculate Term frequency-inverse document frequency (tf-idf) -}
updateSetAndVec :
       Float
    -> String
    -> String
    -> (List (Set String), SparseVector, Index doc)
    -> (List (Set String), SparseVector, Index doc)
updateSetAndVec tf token expandedToken (docSets, vec, Index irec as index) =
    let
      (Index u1irec as u1index, keyIdf) = IndexUtils.idf index expandedToken
      tfidf = tf * keyIdf * (similarityBoost token expandedToken)
      -- _ = Debug.log("updateSetAndVec") (tf, token, expandedToken, (similarityBoost token expandedToken), keyIdf, tfidf)
      -- _ = Debug.log("updateSetAndVec corpus") (irec.corpusTokensIndex)
      u1vec =
        withDefault vec <|
          Maybe.map
            (\pos -> (SparseVector.insert pos tfidf vec))
            (Dict.get token irec.corpusTokensIndex)
      expandedTokenDocSet =
        withDefault Set.empty <|
          Maybe.map
            (\dict -> Set.fromList (Dict.keys dict))
            (Trie.get expandedToken u1irec.tokenStore)
      u1docSets = expandedTokenDocSet :: docSets
      -- _ = Debug.log("updateSetAndVec u1docSets u1vec") (u1docSets, u1vec)
    in
      (u1docSets, u1vec, u1index)


{- if the expanded token is not an exact match to the token then
penalise the score for this key by how different the key is
to the token. -}
similarityBoost : String -> String -> Float
similarityBoost token expandedToken =
    if expandedToken == token then
      1
    else
      1 / (logBase 10
            (toFloat
              (max 3
                ( (String.length expandedToken)
                - (String.length token) ))))


{- calculate the score for each doc  -}
scoreAndCompare
    : SparseVector
    -> String
    -> (Index doc, List (String, Float))
    -> (Index doc, List (String, Float))
scoreAndCompare queryVector ref (index, docs) =
    let
      (u1index, docVector) = getDocVector index ref
      -- _ = Debug.log("scoreAndCompare") (docVector)
    in
      (u1index, (ref, SparseVector.cosineSimilarity queryVector docVector) :: docs)


{- build vector for docRef -}
getDocVector : Index doc -> String -> (Index doc, SparseVector)
getDocVector (Index irec as index) docRef =
    withDefault (index, SparseVector.empty) <|
      Maybe.map
        (\tokenSet ->
            List.foldr
              (updateDocVector docRef)
              (index, SparseVector.empty)
              (Set.toList tokenSet)
        )
        (Dict.get docRef irec.documentStore)


{- reducer for docRef docVector for this token -}
updateDocVector
    : String
    -> String
    -> (Index doc, SparseVector)
    -> (Index doc, SparseVector)
updateDocVector docRef token (Index irec as index, docVector) =
    withDefault (index, docVector) <|
      (Dict.get token irec.corpusTokensIndex) `andThen`
        (\pos ->
          (Trie.get token irec.tokenStore) `andThen`
            (\refs ->
              (Dict.get docRef refs) `andThen`
                (\tf ->
                  let
                    (u1index, idfScore) = IndexUtils.idf index token
                  in
                    Just (u1index, SparseVector.insert pos (tf * idfScore) docVector)
                )
            )
        )


--
-- {-----------------------------------------------------
--
-- Alternate version of updateDocVector above.
-- Still here to think about at the moment.
--
-- -----------------------------------------------------}
-- flipAndThen' = flip Maybe.andThen
-- getThen : (a -> Maybe b) -> Maybe a -> Maybe b
-- getThen = flip Maybe.andThen
--
-- updateDocVector'
--     : String
--     -> String
--     -> (Index doc, SparseVector)
--     -> (Index doc, SparseVector)
-- updateDocVector' docRef token (Index irec as index, docVector) =
--     withDefault (index, docVector) <|
--       Maybe.andThen
--         (Dict.get token irec.corpusTokensIndex)
--         (updateAddPos docRef token index docVector)
--       -- (Dict.get token irec.corpusTokensIndex) `andThen`
--       --   (updateAddPos docRef token index docVector)
--
--
-- updateAddPos docRef token (Index irec as index) docVector pos =
--     Maybe.andThen
--       (Trie.get token irec.tokenStore)
--       (updateAddTf docRef token index docVector pos)
--     -- (Trie.get token irec.tokenStore) `andThen`
--     --   (updateAddTf docRef token index docVector pos)
--     -- (Trie.get token irec.tokenStore)
--     --   |> flipAndThen' (updateAddTf docRef token index docVector pos)
--
--
-- updateAddTf docRef token index docVector pos refs =
--     Maybe.andThen
--       (Dict.get docRef refs)
--       (updateVectorIdf token docVector index pos)
--     -- (Dict.get docRef refs) `andThen`
--     --   (updateVectorIdf token docVector index pos)
--     -- (Dict.get docRef refs)
--     --   |> flipAndThen' (updateVectorIdf token docVector index pos)
--
--
-- updateVectorIdf token docVector index pos tf =
--     let
--       (u1index, idfScore) = IndexUtils.idf index token
--     in
--       Just (u1index, SparseVector.insert pos (tf * idfScore) docVector)
