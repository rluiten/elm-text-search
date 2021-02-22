module TokenProcessorTests exposing (tokenizerTest, tokenizerTests, trimmerTest, trimmerTests)

import Expect
import Test exposing (..)
import TokenProcessors


tokenizerTests : Test
tokenizerTests =
    describe "Lunr TokenProcessors tokenizer tests" <|
        List.map tokenizerTest
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


tokenizerTest : ( String, String, List String ) -> Test
tokenizerTest ( name, testString, expectedTokens ) =
    test name <|
        \() ->
            Expect.equal
                expectedTokens
                (TokenProcessors.tokenizer testString)


trimmerTests : Test
trimmerTests =
    describe "Lunr TokenProcessors trimmer tests" <|
        List.map trimmerTest
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
