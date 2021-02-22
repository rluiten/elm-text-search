module Index.Utils exposing
    ( createFuncCreator
    , getTokens
    , getTokensList
    , processTokens
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
@docs idf
@docs refExists
@docs buildOrderIndex

Copyright (c) 2016 Robin Luiten

-}

import Dict exposing (Dict)
import Index.Model
    exposing
        ( FilterFactory
        , FuncFactory
        , Index(..)
        , TransformFunc
        , TransformFunc2
        )
import Set exposing (Set)
import TokenProcessors
import Trie


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
So filters apply to untokenized words from document.

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
        ( u1index, transformList2 ) =
            getOrSetTransformList index
    in
    ( u1index
    , List.filterMap
        (applyTransformList transformList2)
        strings
    )


{-| Would prefer to pass just accessors (eg .transforms) to
getOrSetIndexFuncList but so far the types are beating me.
-}
getOrSetTransformList : Index doc -> ( Index doc, List TransformFunc2 )
getOrSetTransformList index =
    getOrSetIndexFuncListA
        (\(Index irec) -> irec.transforms)
        (\(Index irec) -> irec.transformFactories)
        setIndexTransforms
        index


{-| set Index transforms func field

Added listFuncs2

-}
setIndexTransforms : Index doc -> List TransformFunc2 -> Index doc
setIndexTransforms (Index irec) listFuncs2 =
    Index { irec | transforms = Just listFuncs2 }


applyInitialTransform : Index doc -> List String -> ( Index doc, List String )
applyInitialTransform index strings =
    let
        ( u1index, intitialTransformList2 ) =
            getOrSetInitialTransformList index
    in
    ( u1index
    , List.filterMap
        (applyTransformList intitialTransformList2)
        strings
    )


getOrSetInitialTransformList : Index doc -> ( Index doc, List TransformFunc2 )
getOrSetInitialTransformList index =
    getOrSetIndexFuncListA
        (\(Index irec) -> irec.initialTransforms)
        (\(Index irec) -> irec.initialTransformFactories)
        setIndexInitialTransforms
        index


setIndexInitialTransforms : Index doc -> List TransformFunc2 -> Index doc
setIndexInitialTransforms (Index irec) listFuncs2 =
    Index { irec | initialTransforms = Just listFuncs2 }


{-| Apply all transforms in sequence to input token.

This works it came from reference learn-maybe/src/Transforms.elm my test project.

-}
applyTransformList : List TransformFunc2 -> String -> Maybe String
applyTransformList transforms token =
    List.foldl (\t -> Maybe.andThen t) (Just token) transforms


{-| Adapt function String -> String
Into String -> Maybe String
Where an empty string maps to Nothing.

This is only exposed to test AUGH!
-}
adaptFuncStrA : a -> (String -> a) -> (String -> Maybe a)
adaptFuncStrA aValue func =
    \string ->
        let
            result =
                func string
        in
        if result /= aValue then
            Just result

        else
            Nothing


adaptFuncStrB : (String -> Bool) -> (String -> Maybe String)
adaptFuncStrB func =
    \string ->
        let
            result =
                func string
        in
        if result then
            Just string

        else
            Nothing


{-| Apply index filters to tokens.

If any token is an empty string it will be filtered out as well.

-}
applyFilter : Index doc -> List String -> ( Index doc, List String )
applyFilter index strings =
    let
        ( u1index, filterList2 ) =
            getOrSetFilterList index
    in
    ( u1index
    , List.filterMap
        (applyTransformList filterList2)
        strings
    )


getOrSetFilterList : Index doc -> ( Index doc, List TransformFunc2 )
getOrSetFilterList index =
    getOrSetIndexFuncListB
        (\(Index irec) -> irec.filters)
        (\(Index irec) -> irec.filterFactories)
        setIndexFilters
        index


{-| set Index filters func field
-}
setIndexFilters : Index doc -> List TransformFunc2 -> Index doc
setIndexFilters (Index irec) listFuncs2 =
    Index { irec | filters = Just listFuncs2 }


{-| String TranformFunc source type variant.

See getOrSetIndexFuncListB for FilterFunc variant
Generic type `a` isnt helping me here so splitting for specific types
Dang and these two variants work.

-}
getOrSetIndexFuncListA :
    (Index doc -> Maybe (List TransformFunc2))
    -> (Index doc -> List (FuncFactory doc TransformFunc))
    -> (Index doc -> List TransformFunc2 -> Index doc)
    -> Index doc
    -> ( Index doc, List TransformFunc2 )
getOrSetIndexFuncListA getFuncs2 getFactoryFuncs setFuncs index =
    case getFuncs2 index of
        -- init allready run
        Just funcList2 ->
            ( index, funcList2 )

        -- rebuild function lists
        _ ->
            let
                ( u1index, newFuncList ) =
                    runFactories (getFactoryFuncs index) index

                newFunc2List =
                    List.map (adaptFuncStrA "") newFuncList

                u2index =
                    setFuncs u1index newFunc2List
            in
            ( u2index, newFunc2List )


{-| Variant for FilterFunc hydration

If i switch FilterFunc to be TransformFunc instead i can share above code, just one less variation.

-}
getOrSetIndexFuncListB :
    (Index doc -> Maybe (List TransformFunc2))
    -> (Index doc -> List (FilterFactory doc))
    -> (Index doc -> List TransformFunc2 -> Index doc)
    -> Index doc
    -> ( Index doc, List TransformFunc2 )
getOrSetIndexFuncListB getFuncs2 getFactoryFuncs setFuncs index =
    case getFuncs2 index of
        -- init allready run
        Just funcList2 ->
            ( index, funcList2 )

        -- rebuild function lists
        _ ->
            let
                ( u1index, newFuncList ) =
                    runFactories (getFactoryFuncs index) index

                newFunc2List =
                    List.map adaptFuncStrB newFuncList

                u2index =
                    setFuncs u1index newFunc2List
            in
            ( u2index, newFunc2List )


{-| Run each of the function factories returning the list of functions.

TODO use foldr?, probably dont mater here

-}
runFactories : List (FuncFactory doc func) -> Index doc -> ( Index doc, List func )
runFactories factoryList index =
    List.foldr
        (\factory ( u1index, funcList ) ->
            let
                ( u2index, newFunc ) =
                    factory u1index
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
    case Dict.get token irec.idfCache of
        Nothing ->
            calcIdf index token

        Just idfValue ->
            ( index, idfValue )


calcIdf : Index doc -> String -> ( Index doc, Float )
calcIdf (Index irec) token =
    let
        -- _ = Debug.log("calcIdf") (token)
        docFrequency =
            toFloat (Trie.valueCount token irec.tokenStore)

        idfLocal =
            if docFrequency > 0 then
                1
                    + logBase 10
                        (toFloat (Dict.size irec.documentStore) / docFrequency)

            else
                toFloat 1

        updatedIdfCache =
            Dict.insert token idfLocal irec.idfCache

        u1index =
            Index
                { irec
                    | idfCache = updatedIdfCache
                }
    in
    ( u1index, idfLocal )


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
            List.indexedMap Tuple.pair (Set.toList tokenSet)
    in
    List.foldr (\( i, v ) d -> Dict.insert v i d) Dict.empty withIndex
