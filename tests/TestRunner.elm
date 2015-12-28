module TestRunner where

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)

import TokenProcessorTests
import IndexTests
import IndexUtilsTests
import StopWordFilterTests
import IndexEncoderTests
import IndexDecoderTests
import IndexLoadTests


-- These are imported just to make sure they compile
import Examples.LunrelmNew
import Examples.LunrelmNewWith


import Index
import IndexModel exposing (..)


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
        ]
      )
