module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Color(..), Feedback, Hint(..), Row(..), mkFeedback)
import Maybe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Scoring"
        [ describe "mkFeedback"
            [ test "all the same color" <|
                \_ ->
                    let
                        guess =
                            Main.Row Red Red Red Red

                        pick =
                            Main.Row Red Red Red Red

                        actual =
                            mkFeedback guess pick

                        expected =
                            { correctColorPosition = 4
                            , correctColor = 0
                            , empty = 0
                            }
                    in
                    Expect.equal expected actual
            ]
        , describe "detectCorrectPosition"
            [ test "all the same color" <|
                \_ ->
                    let
                        guess =
                            Main.Row Red Red Red Red

                        pick =
                            Main.Row Red Red Red Red

                        actual =
                            Main.detectCorrectPosition guess pick

                        expected =
                            ( 4, [] )
                    in
                    Expect.equal expected actual
            ]
        ]
