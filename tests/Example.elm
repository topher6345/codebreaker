module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Color(..), Row(..), mkFeedback)
import Maybe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Feedback"
        [ describe "mkFeedback"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        actual =
                            mkFeedback
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))
                        actual =
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))
                    in
                    Expect.equal result result
            ]
        ]
