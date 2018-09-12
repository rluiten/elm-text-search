module Index.Vector exposing (buildDocVector, getDocVector, getQueryVector, scoreAndCompare, similarityBoost, updateDocVector, updateSetAndVec)

{-| Index document vector support.

Copyright (c) 2016 Robin Luiten

-}

import Dict exposing (Dict)
import Index.Model exposing (Index(..))
import Index.Utils
import Maybe
import Set exposing (Set)
import SparseVector exposing (SparseVector)
import String
import Trie exposing (Trie)


{-| Build a query vector and the sets of candidate document matches
for each token in our query tokens.

Each token in our query will have a seperate Set String entry in
the returned List. As all query token document result sets are
intersected together for final list of documents matched. (a logical and
of all the query tokens)

-}
getQueryVector :
    Float
    -> List String
    -> Index doc
    -> ( List (Set String), SparseVector, Index doc )
getQueryVector fieldBoosts tokens index =
    List.foldr
        (buildDocVector (List.length tokens) fieldBoosts)
        ( [], SparseVector.empty, index )
        tokens


{-| Update query vector elements to create query vector.
Update the list of documents that match for each query token (baseToken).
-}
buildDocVector :
    Int
    -> Float
    -> String
    -> ( List (Set String), SparseVector, Index doc )
    -> ( List (Set String), SparseVector, Index doc )
buildDocVector tokensLength fieldBoosts baseToken ( docSets, vec, (Index irec) as index ) =
    let
        termFrequency =
            1
                / toFloat tokensLength
                * toFloat (List.length irec.fields)
                * fieldBoosts

        expandedTokens =
            Trie.expand baseToken irec.tokenStore

        -- _ = Debug.log("buildDocVector") (tokensLength, baseToken, expandedTokens)
        ( docs, vecU1, indexU1 ) =
            List.foldr
                (updateSetAndVec termFrequency baseToken)
                ( Set.empty, vec, index )
                expandedTokens
    in
    ( docs :: docSets, vecU1, indexU1 )


{-| Calculate Term frequency-inverse document frequency (tf-idf).
Union of documents for each expandedToken for this (base)token.
-}
updateSetAndVec :
    Float
    -> String
    -> String
    -> ( Set String, SparseVector, Index doc )
    -> ( Set String, SparseVector, Index doc )
updateSetAndVec tf token expandedToken ( docSets, vec, (Index irec) as index ) =
    let
        ( (Index u1irec) as u1index, keyIdf ) =
            Index.Utils.idf index expandedToken

        tfidf =
            tf * keyIdf * similarityBoost token expandedToken

        -- _ = Debug.log("updateSetAndVec") (tf, token, expandedToken, (similarityBoost token expandedToken), keyIdf, tfidf)
        -- _ = Debug.log("updateSetAndVec corpus") (irec.corpusTokensIndex)
        u1vec =
            Maybe.withDefault vec <|
                Maybe.map
                    (\pos -> SparseVector.insert pos tfidf vec)
                    (Dict.get expandedToken irec.corpusTokensIndex)

        expandedTokenDocSet =
            Maybe.withDefault Set.empty <|
                Maybe.map
                    (\dict -> Set.fromList (Dict.keys dict))
                    (Trie.get expandedToken u1irec.tokenStore)

        u1docSets =
            Set.union expandedTokenDocSet docSets

        -- _ = Debug.log("updateSetAndVec u1docSets u1vec") (expandedToken, u1docSets, u1vec)
    in
    ( u1docSets, u1vec, u1index )


{-| if the expanded token is not an exact match to the token then
penalise the score for this key by how different the key is
to the token.
-}
similarityBoost : String -> String -> Float
similarityBoost token expandedToken =
    if expandedToken == token then
        1

    else
        1
            / logBase 10
                (toFloat
                    (max 3
                        (String.length expandedToken
                            - String.length token
                        )
                    )
                )


{-| calculate the score for each doc
-}
scoreAndCompare :
    SparseVector
    -> String
    -> ( Index doc, List ( String, Float ) )
    -> ( Index doc, List ( String, Float ) )
scoreAndCompare queryVector ref ( index, docs ) =
    let
        ( u1index, docVector ) =
            getDocVector index ref

        -- _ = Debug.log("scoreAndCompare") (docVector)
    in
    ( u1index, ( ref, SparseVector.cosineSimilarity queryVector docVector ) :: docs )


{-| build vector for docRef
-}
getDocVector : Index doc -> String -> ( Index doc, SparseVector )
getDocVector ((Index irec) as index) docRef =
    Maybe.withDefault ( index, SparseVector.empty ) <|
        Maybe.map
            (\tokenSet ->
                List.foldr
                    (updateDocVector docRef)
                    ( index, SparseVector.empty )
                    (Set.toList tokenSet)
            )
            (Dict.get docRef irec.documentStore)


{-| reducer for docRef docVector for this token
-}
updateDocVector : String -> String -> ( Index doc, SparseVector ) -> ( Index doc, SparseVector )
updateDocVector docRef token (( (Index irec) as index, docVector ) as inputTuple) =
    Maybe.withDefault inputTuple <|
        Maybe.map2
            (\position termFrequency ->
                let
                    ( u1index, idfScore ) =
                        Index.Utils.idf index token
                in
                ( u1index, SparseVector.insert position (termFrequency * idfScore) docVector )
            )
            (Dict.get token irec.corpusTokensIndex)
            (Trie.get token irec.tokenStore
                |> Maybe.andThen (Dict.get docRef)
            )
