module Index.Utils
    exposing
        ( createFuncCreator
        , getTokens
        , getTokensList
        , processTokens
        , applyTransform
        , applyFilter
        , idf
        , refExists
        , buildOrderIndex
        )

{-| Index Utilities


## Functions

@docs createFuncCreator
@docs getTokens
@docs getTokensList
@docs processTokens
@docs applyTransform
@docs applyFilter
@docs idf
@docs refExists
@docs buildOrderIndex

Copyright (c) 2016-2017 Robin Luiten

-}

import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)
import Trie exposing (Trie)
import Set exposing (Set)
import Index.Model exposing (Index(Index), FuncFactory, FilterFactory)
import TokenProcessors


{-| Create a function creator (FuncFactory)
given the simple Function to start with
-}
createFuncCreator : func -> FuncFactory doc func
createFuncCreator func index =
    ( index, func )


{-| Extract tokens from string, and process them.
-}
getTokens : Index doc -> String -> ( Index doc, List String )
getTokens index string =
    processTokens index (TokenProcessors.tokenizer string)


getTokensList : Index doc -> List String -> ( Index doc, List String )
getTokensList index listString =
    processTokens index (TokenProcessors.tokenizerList listString)


{-| Transform list of words into tokens for index and search.

Applies filters and transformers configured in index.

Applies filters first then tokenizers.
So filtesr applie to untokenized words from document.

-}
processTokens : Index doc -> List String -> ( Index doc, List String )
processTokens index tokens =
    let
        ( u1index, initialTransformTokens ) =
            applyInitialTransform index tokens

        ( u2index, filterTokens ) =
            applyFilter u1index initialTransformTokens
    in
        applyTransform u2index filterTokens


{-| Apply the transforms to tokens.
If any transform converts a token to an empty string no further transforms
are applied and the empty string is removed from the set of tokens.
-}
applyTransform : Index doc -> List String -> ( Index doc, List String )
applyTransform index strings =
    let
        ( u1index, transformList ) =
            getOrSetTransformList index
    in
        ( u1index
        , (List.filter
            (\val -> val /= "")
            (List.map (applyTransformList transformList) strings)
          )
        )


{-| Would prefer to past just accessors (eg .transforms) to
getOrSetIndexFuncList but so far the types are beating me.
-}
getOrSetTransformList : Index doc -> ( Index doc, List (String -> String) )
getOrSetTransformList index =
    getOrSetIndexFuncList
        (\(Index irec) -> irec.transforms)
        (\(Index irec) -> irec.transformFactories)
        setIndexTransforms
        index


{-| set Index transforms func field
-}
setIndexTransforms : Index doc -> List (String -> String) -> Index doc
setIndexTransforms (Index irec) listFuncs =
    Index { irec | transforms = Just listFuncs }


applyInitialTransform : Index doc -> List String -> ( Index doc, List String )
applyInitialTransform index strings =
    let
        ( u1index, intitialTransformList ) =
            getOrSetInitialTransformList index
    in
        ( u1index
        , (List.filter
            (\val -> val /= "")
            (List.map (applyTransformList intitialTransformList) strings)
          )
        )


getOrSetInitialTransformList : Index doc -> ( Index doc, List (String -> String) )
getOrSetInitialTransformList index =
    getOrSetIndexFuncList
        (\(Index irec) -> irec.initialTransforms)
        (\(Index irec) -> irec.initialTransformFactories)
        setIndexInitialTransforms
        index


setIndexInitialTransforms : Index doc -> List (String -> String) -> Index doc
setIndexInitialTransforms (Index irec) listFuncs =
    Index { irec | initialTransforms = Just listFuncs }


{-| Apply all transforms in sequence to input token.

If any transform returns an empty string then this will return
the empty string without running further transforms.

-}
applyTransformList : List (String -> String) -> String -> String
applyTransformList transforms token =
    case transforms of
        [] ->
            token

        transform :: restTransforms ->
            let
                newToken =
                    transform token
            in
                case newToken of
                    "" ->
                        ""

                    _ ->
                        applyTransformList restTransforms newToken


{-| Apply index filters to tokens.

If any token is an empty string it will be filtered out as well.

-}
applyFilter : Index doc -> List String -> ( Index doc, List String )
applyFilter index strings =
    let
        ( u1index, filterList ) =
            getOrSetFilterList index
    in
        ( u1index, List.filter (applyFilterList filterList) strings )


getOrSetFilterList : Index doc -> ( Index doc, List (String -> Bool) )
getOrSetFilterList index =
    getOrSetIndexFuncList
        (\(Index irec) -> irec.filters)
        (\(Index irec) -> irec.filterFactories)
        setIndexFilters
        index


{-| set Index filters func field
-}
setIndexFilters : Index doc -> List (String -> Bool) -> Index doc
setIndexFilters (Index irec) listFuncs =
    Index { irec | filters = Just listFuncs }


{-| If any filter returns False then return False.

Place more descriminant filters as early as possible in filters
list as they are run in order.

-}
applyFilterList : List (String -> Bool) -> String -> Bool
applyFilterList filters token =
    case filters of
        [] ->
            True

        filterFunc :: restFilters ->
            case token of
                "" ->
                    False

                _ ->
                    case filterFunc token of
                        False ->
                            False

                        True ->
                            applyFilterList restFilters token


{-| Get a list of functions from Index, if they have not been created
they are created and set on Index.
-}
getOrSetIndexFuncList :
    (Index doc -> Maybe (List func))
    -> (Index doc -> List (FuncFactory doc func))
    -> (Index doc -> List func -> Index doc)
    -> Index doc
    -> ( Index doc, List func )
getOrSetIndexFuncList getFuncs getFactoryFuncs setFuncs index =
    case (getFuncs index) of
        Just funcList ->
            ( index, funcList )

        Nothing ->
            let
                ( u1index, newFuncList ) =
                    runFactories (getFactoryFuncs index) index

                u2index =
                    setFuncs u1index newFuncList
            in
                ( u2index, newFuncList )


{-| Run each of the function factories returning the list of functions.
-}
runFactories : List (FuncFactory doc func) -> Index doc -> ( Index doc, List func )
runFactories factoryList index =
    List.foldr
        (\factory ( u1index, funcList ) ->
            let
                ( u2index, newFunc ) =
                    (factory u1index)
            in
                ( u2index, newFunc :: funcList )
        )
        ( index, [] )
        factoryList


{-| Calculate the inverse document frequency for a token in the Index.

Model will update if token has no cached value for idf.

-}
idf : Index doc -> String -> ( Index doc, Float )
idf ((Index irec) as index) token =
    case (Dict.get token irec.idfCache) of
        Nothing ->
            calcIdf index token

        Just idf ->
            ( index, idf )


calcIdf : Index doc -> String -> ( Index doc, Float )
calcIdf (Index irec) token =
    let
        -- _ = Debug.log("calcIdf") (token)
        docFrequency =
            toFloat (Trie.valueCount token irec.tokenStore)

        idf =
            if docFrequency > 0 then
                1
                    + logBase 10
                        (toFloat (Dict.size irec.documentStore) / docFrequency)
            else
                toFloat 1

        updatedIdfCache =
            Dict.insert token idf irec.idfCache

        u1index =
            Index
                { irec
                    | idfCache = updatedIdfCache
                }
    in
        ( u1index, idf )


{-| Return True if document reference is indexed.
-}
refExists : String -> Index doc -> Bool
refExists docRef (Index irec) =
    Dict.member docRef irec.documentStore


{-| Build an index of string to index from Set where key is
Set word and value is ordered index of word in Set.
-}
buildOrderIndex : Set String -> Dict String Int
buildOrderIndex tokenSet =
    let
        withIndex =
            List.indexedMap (,) (Set.toList tokenSet)
    in
        List.foldr (\( i, v ) d -> Dict.insert v i d) Dict.empty withIndex
