module Example exposing (suite)

import Expect
import Main exposing (Color(..), Hint(..), Row(..), detectCorrectPosition, mkFeedback)
import Test exposing (Test, describe, test)


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
            , test "Green Green Red Red" <|
                \_ ->
                    let
                        guess =
                            Main.Row Green Green Yellow Yellow

                        pick =
                            Main.Row Green Green Red Red

                        actual =
                            mkFeedback guess pick

                        expected =
                            { correctColorPosition = 2
                            , correctColor = 0
                            , empty = 2
                            }
                    in
                    Expect.equal expected actual
            , test "White Blue White Red" <|
                \_ ->
                    let
                        guess =
                            Main.Row Red Red Blue Blue

                        pick =
                            Main.Row White Blue White Red

                        actual =
                            mkFeedback guess pick

                        expected =
                            { correctColorPosition = 0
                            , correctColor = 2
                            , empty = 2
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
                            detectCorrectPosition guess pick

                        expected =
                            ( 4, [] )
                    in
                    Expect.equal expected actual
            ]
        ]
