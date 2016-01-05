module TestRunner where

import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import String

import IndexDecoderTests
import IndexEncoderTests
import IndexLoadTests
import IndexTests
import IndexUtilsTests
import SaveLoadTests
import StopWordFilterTests
import TokenProcessorTests

import Index
import Index.Model exposing (..)


main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests"
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
      )
