module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Color(..), Feedback(..), Hint(..), Row(..), mkFeedback)
import Maybe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Feedback"
        [ describe "mkFeedback"
            -- Nest as many descriptions as you like.
            [ test " all the same" <|
                \_ ->
                    let
                        actual =
                            mkFeedback
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))
                                (Main.Row (Just Red) (Just Red) (Just Red) (Just Red))

                        expected =
                            Feedback CorrectColorPosition CorrectColorPosition CorrectColorPosition CorrectColorPosition
                    in
                    Expect.equal expected actual
            ]
        ]
