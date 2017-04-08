module TestUtils exposing (..)

import Expect
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
