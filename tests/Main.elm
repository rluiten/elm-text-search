port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import IndexDecoderTests
import IndexEncoderTests
import IndexLoadTests
import IndexTests
import IndexUtilsTests
import SaveLoadTests
import StopWordFilterTests
import TokenProcessorTests


main : TestProgram
main =
  run emit <|
    describe "Test for elm-text-search"
      [ TokenProcessorTests.tests
      , IndexTests.tests
      , IndexUtilsTests.tests
      -- StopWordFilterTests.tests checks all the
      -- stop words (119 at moment)
      -- so bumps the test count a lot.
      , StopWordFilterTests.tests
      , IndexEncoderTests.tests
      , IndexDecoderTests.tests
      , IndexLoadTests.tests
      , SaveLoadTests.tests
      ]


port emit : ( String, Value ) -> Cmd msg
