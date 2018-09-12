module TestUtils exposing (expectErr, expectOk, expectResultFailureMessage, mapDecodeErrorToString)

import Expect
import Json.Decode as Decode exposing (errorToString, Error(..))
import Test exposing (..)


expectOk : Result String a -> Expect.Expectation
expectOk result =
    case result of
        Ok _ ->
            Expect.true "Result Ok as expected" True

        Err _ ->
            Expect.false "Result Err not expected" False


expectErr : Result String a -> Expect.Expectation
expectErr result =
    case result of
        Ok _ ->
            Expect.true "Result Ok not expected" False

        Err _ ->
            Expect.false "Result Err as expected" True


{-| Map a Decoder Error to a string for our usage, useful ? put in Decoder or ElmTextSearch?
to maintain backward compatibility with interface ? hmm ?
-}
mapDecodeErrorToString : Result Decode.Error a -> Result String a
mapDecodeErrorToString result =
    case result of
        Ok value ->
            Ok value

        Err error ->
            Err (errorToString error)


expectResultFailureMessage : String -> String -> Result Decode.Error x -> Test
expectResultFailureMessage name expectedMessage result =
    test name <|
        \() ->
            Expect.equal expectedMessage
                (case result of
                    Err (Failure description _) ->
                        description

                    _ ->
                        "-= Unexpected Value =- in test " ++ name
                )
