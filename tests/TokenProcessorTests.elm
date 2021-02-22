module TokenProcessorTests exposing (TokenArrayCase, TokenCase, tests, tokenCases, tokenizerTest, trimmerCases, trimmerTest)

import Expect
import Test exposing (..)
import TokenProcessors


type alias TokenCase =
    ( String, String, List String )


type alias TokenArrayCase =
    ( String, List String, List String )


tests : Test
tests =
    describe "Lunr TokenProcessors tests" <|
        (List.map tokenizerTest tokenCases
            ++ -- (List.map tokenizerArrayTest tokenArrayCases)
               -- ++
               List.map trimmerTest trimmerCases
        )


tokenCases : List TokenCase
tokenCases =
    [ ( "splitting simple strings into tokens"
      , "this is a simple string"
      , [ "this", "is", "a", "simple", "string" ]
      )
    , ( "downcasing tokens"
      , "FOO BAR"
      , [ "foo", "bar" ]
      )
    , ( "splitting strings with hyphens"
      , "take the New York-San Francisco flight"
      , [ "take", "the", "new", "york", "san", "francisco", "flight" ]
      )
    , ( "splitting strings with hyphens and spaces"
      , "Solve for A - B"
      , [ "solve", "for", "a", "b" ]
      )
    , ( "leading - in query should not cause extra token ?"
      , "-misery! .appeal,"
      , [ "misery!", ".appeal," ]
      )
    ]


tokenizerTest : TokenCase -> Test
tokenizerTest ( name, testString, expectedTokens ) =
    test name <|
        \() ->
            Expect.equal
                expectedTokens
                (TokenProcessors.tokenizer testString)



-- tokenArrayCases : List TokenArrayCase
-- tokenArrayCases =
--     [ ( "downcasing token arrays"
--       , [ "Foo", "BAR" ]
--       , [ "foo", "bar" ]
--       )
--     ]
--
--
-- tokenizerArrayTest : TokenArrayCase -> Test
-- tokenizerArrayTest (name, testArray, expectedTokens) =
--     test name
--       <| assertEqual
--           expectedTokens
--           (TokenProcessors.tokenizerArray testArray)


trimmerCases : List ( String, String )
trimmerCases =
    [ ( "023hello", "023hello" )
    , ( "=hello", "hello" )
    , ( "hello.", "hello" )
    , ( ",hello,", "hello" )
    , ( ",hello_,", "hello_" )
    , ( "40%", "40" )
    ]


trimmerTest : ( String, String ) -> Test
trimmerTest ( testString, expectedString ) =
    test ("trimmer " ++ testString ++ " -> " ++ expectedString) <|
        \() ->
            Expect.equal
                expectedString
                (TokenProcessors.trimmer testString)
