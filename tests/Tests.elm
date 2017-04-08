module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import IndexDecoderTests
import IndexEncoderTests
import IndexLoadTests
import IndexTests
import IndexUtilsTests
import SaveLoadTests
import StopWordFilterTests
import TokenProcessorTests


all : Test
all =
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
