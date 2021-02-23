module TestUtils exposing
    ( expectOkWithGoodFailMessage
    , getDecodeErrorFailureMessage
    , getErrorIgnoreResult
    , getResultIgnoreError
    , isErr
    , isOk
    )

{-| Utilities to make test cases simpler.
-}

import Expect
import Index
import Index.Model exposing (Index(..))
import Json.Decode exposing (Error(..))
import Test exposing (..)


expectOkWithGoodFailMessage : Result Error a -> Expect.Expectation
expectOkWithGoodFailMessage result =
    case result of
        Ok _ ->
            Expect.true "Result Ok as expected" True

        Err error ->
            Expect.false
                (String.concat
                    [ "Result Err not expected: "
                    , getDecodeErrorFailureMessage error
                    ]
                )
                True


getResultIgnoreError : Result error a -> a
getResultIgnoreError result =
    case result of
        Ok value ->
            value

        Err _ ->
            Debug.todo "Ignoring failure for testing"


getErrorIgnoreResult : Result error a -> error
getErrorIgnoreResult result =
    case result of
        Ok _ ->
            Debug.todo "Ignoring value for testing"

        Err error ->
            error


getDecodeErrorFailureMessage : Error -> String
getDecodeErrorFailureMessage error =
    case error of
        Failure message _ ->
            message

        _ ->
            Debug.todo "Ignoring all but Feailures of Decode Error"


isOk : Result e a -> Bool
isOk x =
    case x of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result e a -> Bool
isErr x =
    case x of
        Ok _ ->
            False

        Err _ ->
            True
