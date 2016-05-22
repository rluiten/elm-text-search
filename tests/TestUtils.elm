module TestUtils exposing (..)

import ElmTest exposing (..)


assertOk : Result String a -> Assertion
assertOk result =
    case result of
      Ok _ -> assert True
      Err _ -> assert False


assertErr : Result String a -> Assertion
assertErr result =
    case result of
      Ok _ -> assert False
      Err _ -> assert True
