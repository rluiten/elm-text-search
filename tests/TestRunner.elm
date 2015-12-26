module TestRunner where

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)

import TokenProcessorTests
import IndexTests
import IndexUtilsTests
import StopWordFilterTests

-- These are imported just to make sure they compile
import Lunrelm
import LunrelmNew
import LunrelmNewWith

main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests"
        [ TokenProcessorTests.tests
        , IndexTests.tests
        , IndexUtilsTests.tests
        , StopWordFilterTests.tests
        ]
      )
