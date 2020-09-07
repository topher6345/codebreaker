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
            [ test " all the same" <|
                \_ ->
                    let
                        guess =
                            Main.Row (Just Red) (Just Red) (Just Red) (Just Red)

                        pick =
                            Main.Row (Just Red) (Just Red) (Just Red) (Just Red)

                        actual =
                            mkFeedback guess pick

                        expected =
                            Feedback CorrectColorPosition CorrectColorPosition CorrectColorPosition CorrectColorPosition
                    in
                    Expect.equal expected actual
            , test "one of each color" <|
                \_ ->
                    let
                        pick =
                            Main.Row (Just Green) (Just Red) (Just Blue) (Just Yellow)

                        guess =
                            Main.Row (Just Green) (Just Red) (Just Blue) (Just Yellow)

                        actual =
                            mkFeedback guess pick

                        expected =
                            Feedback CorrectColorPosition CorrectColorPosition CorrectColorPosition CorrectColorPosition
                    in
                    Expect.equal expected actual
            ]
        ]
