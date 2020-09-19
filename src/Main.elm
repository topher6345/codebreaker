module Main exposing (Color(..), Either(..), Feedback(..), Hint(..), HintTable, Row(..), main, mkFeedback, zipRow)

import Array exposing (Array(..), fromList)
import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe(..))
import Random
import String


type alias Model =
    { currentRound : Int
    , row : Row
    , guesses : Array ( Feedback, Row )
    , pick : Row
    , reveal : Bool
    , flash : String
    }


type Color
    = Red
    | Blue
    | Green
    | Yellow
    | Pink
    | White
    | None


colorShow color =
    case color of
        Red ->
            "\u{1F7E5}"

        Blue ->
            "\u{1F7E6}"

        Green ->
            "\u{1F7E9}"

        Yellow ->
            "\u{1F7E8}"

        Pink ->
            "\u{1F7EA}"

        White ->
            "⬜️"

        None ->
            ""


mkColor string =
    case string of
        "\u{1F7E5}" ->
            Red

        "\u{1F7E6}" ->
            Blue

        "\u{1F7E9}" ->
            Green

        "\u{1F7E8}" ->
            Yellow

        "\u{1F7EA}" ->
            Pink

        "⬜️" ->
            White

        _ ->
            None


type Row
    = Row Color Color Color Color


blankRow =
    Row None None None None


nonEmptyRow (Row a b c d) =
    List.all ((/=) None) [ a, b, c, d ]


type RowIndex
    = First
    | Second
    | Third
    | Fourth


updateRow row rowIndex color =
    case ( rowIndex, row ) of
        ( First, Row a b c d ) ->
            Row color b c d

        ( Second, Row a b c d ) ->
            Row a color c d

        ( Third, Row a b c d ) ->
            Row a b color d

        ( Fourth, Row a b c d ) ->
            Row a b c color


getFromRow row rowIndex =
    case ( rowIndex, row ) of
        ( First, Row a _ _ _ ) ->
            a

        ( Second, Row _ b _ _ ) ->
            b

        ( Third, Row _ _ c _ ) ->
            c

        ( Fourth, Row _ _ _ d ) ->
            d


updateRowColor guesses rowIndex string colIndex =
    let
        ( hint, row ) =
            Array.get colIndex guesses
                |> Maybe.withDefault ( initFeedback, blankRow )
    in
    case mkColor string of
        None ->
            ( hint, updateRow row rowIndex None )

        color ->
            ( hint, updateRow row rowIndex color )


type Hint
    = CorrectColorPosition
    | CorrectColor
    | Empty


showHint hint =
    case hint of
        CorrectColorPosition ->
            "◾️"

        CorrectColor ->
            "◽️"

        Empty ->
            "☐"


type Feedback
    = Feedback Hint Hint Hint Hint


initFeedback =
    Feedback Empty Empty Empty Empty


win =
    Feedback CorrectColorPosition CorrectColorPosition CorrectColorPosition CorrectColorPosition


initGuesses =
    Array.repeat 8 ( initFeedback, blankRow )


initialModel : Model
initialModel =
    { currentRound = 0
    , row = blankRow
    , pick = pickToRow (Pick Red Red Red Red)
    , guesses = initGuesses
    , flash = ""
    , reveal = False
    }


type Msg
    = UpdateColor RowIndex Int String
    | Roll Pick
    | Submit
    | Cheat


type alias HintTable =
    { correctColorPosition : Int
    , correctColor : Int
    , empty : Int
    }


mkFeedback actual expected =
    let
        get i =
            Array.get i list |> Maybe.withDefault Empty

        list =
            Array.fromList (detectCorrectPosition actual expected ++ detectCorrectColor actual expected)

        a =
            get 0

        b =
            get 1

        c =
            get 2

        d =
            get 3
    in
    Feedback a b c d


detectCorrectPosition : Row -> Row -> List Hint
detectCorrectPosition (Row a b c d) (Row e f g h) =
    let
        list =
            [ a == e, b == f, c == g, d == h ]

        fmap x =
            if x then
                Just CorrectColorPosition

            else
                Nothing
    in
    List.filterMap fmap list


zipRow (Row a b c d) (Row e f g h) =
    [ ( a, e ), ( b, f ), ( c, g ), ( d, h ) ]


count predicate list =
    List.foldr
        (\a b ->
            if predicate a then
                b + 1

            else
                b
        )
        0
        list


type Either a b
    = Left a
    | Right b


transpose rows =
    ( List.map Tuple.first rows, List.map Tuple.second rows )


detectCorrectColor (Row a b c d) (Row e f g h) =
    let
        list =
            [ List.member a [ f, g, h ]
            , List.member b [ e, g, d ]
            , List.member c [ e, f, h ]
            , List.member d [ e, f, g ]
            ]

        fmap x =
            if x then
                Just CorrectColor

            else
                Nothing
    in
    List.filterMap fmap list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateColor rowIndex colIndex string ->
            ( { model
                | guesses = Array.set colIndex (updateRowColor model.guesses rowIndex string colIndex) model.guesses
              }
            , Cmd.none
            )

        Roll pick ->
            ( { model | pick = pickToRow pick }, Cmd.none )

        Submit ->
            let
                index =
                    model.currentRound

                ( _, row ) =
                    model.guesses
                        |> Array.get index
                        |> Maybe.withDefault ( initFeedback, blankRow )

                feedback =
                    mkFeedback row model.pick

                newGuesses =
                    Array.set index ( feedback, row ) model.guesses

                currentRound =
                    model.currentRound + 1
            in
            case ( feedback == win, currentRound > 8 ) of
                ( True, False ) ->
                    ( { model | flash = "You win!", currentRound = currentRound, guesses = newGuesses }, Cmd.none )

                ( False, True ) ->
                    ( { model | flash = "You Lose", currentRound = currentRound, guesses = newGuesses }, Cmd.none )

                _ ->
                    ( { model | currentRound = currentRound, guesses = newGuesses }, Cmd.none )

        Cheat ->
            ( { model | reveal = True }, Cmd.none )


choice guesses rowIndex disabled colIndex =
    let
        ( _, row2 ) =
            Array.get colIndex guesses |> Maybe.withDefault ( initFeedback, blankRow )
    in
    Html.select
        ([ Html.Attributes.value (getFromRow row2 rowIndex |> colorShow) ]
            ++ (if disabled then
                    [ attribute "disabled" "true" ]

                else
                    [ onInput (UpdateColor rowIndex colIndex) ]
               )
        )
        [ Html.option
            [ attribute "selected" ""
            , attribute "value" ""
            ]
            [ text "-" ]
        , Html.option [] [ text "\u{1F7E5}" ]
        , Html.option [] [ text "\u{1F7E6}" ]
        , Html.option [] [ text "\u{1F7E9}" ]
        , Html.option [] [ text "\u{1F7E8}" ]
        , Html.option [] [ text "\u{1F7EA}" ]
        , Html.option [] [ text "⬜️" ]
        ]


hintTable (Feedback a b c d) =
    [ table []
        [ tbody []
            [ tr []
                [ td [] [ text <| showHint a ]
                , td [] [ text <| showHint b ]
                ]
            , tr []
                [ td [] [ text <| showHint c ]
                , td [] [ text <| showHint d ]
                ]
            ]
        ]
    ]


submitable guesses index colIndex =
    let
        ( _, row ) =
            Array.get index guesses
                |> Maybe.withDefault ( initFeedback, blankRow )
    in
    if nonEmptyRow row && index == colIndex then
        [ onClick Submit ]

    else
        [ attribute "disabled" "true" ]


mkHintTable index guesses =
    case Array.get index guesses of
        Just ( feedback, _ ) ->
            hintTable feedback

        Nothing ->
            []


hintsTr guesses =
    tr [] (List.range 0 7 |> List.map (\i -> td [] <| mkHintTable i guesses))


guessesTds rowIndex currentRound guesses =
    let
        mkTd index =
            td [] [ choice guesses rowIndex (currentRound /= index) index ]
    in
    List.range 0 7
        |> List.map mkTd


mkSubmitRows guesses currentRound =
    List.range 0 7 |> List.map (\i -> td [] [ button (submitable guesses currentRound i) [ text <| String.fromInt (i + 1) ] ])


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Guessing Game" ]
        , Html.p [] [ text model.flash ]
        , table []
            [ tbody []
                [ hintsTr model.guesses ]
            , tbody []
                [ tr [] <|
                    guessesTds First model.currentRound model.guesses
                        ++ [ td []
                                (if model.reveal then
                                    [ text <| colorShow <| (\(Row a _ _ _) -> a) model.pick ]

                                 else
                                    []
                                )
                           ]
                , tr [] <|
                    guessesTds Second model.currentRound model.guesses
                        ++ [ td []
                                (if model.reveal then
                                    [ text <| colorShow <| (\(Row _ b _ _) -> b) model.pick ]

                                 else
                                    []
                                )
                           ]
                , tr [] <|
                    guessesTds Third model.currentRound model.guesses
                        ++ [ td []
                                (if model.reveal then
                                    [ text <| colorShow <| (\(Row _ _ c _) -> c) model.pick ]

                                 else
                                    []
                                )
                           ]
                , tr [] <|
                    guessesTds Fourth model.currentRound model.guesses
                        ++ [ td []
                                (if model.reveal then
                                    [ text <| colorShow <| (\(Row _ _ _ d) -> d) model.pick ]

                                 else
                                    []
                                )
                           ]
                , tr [] <|
                    mkSubmitRows model.guesses model.currentRound
                        ++ [ td [] [ button [ onClick Cheat ] [ text "Cheat" ] ]
                           ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Random.generate Roll roll
    )


colors =
    Array.fromList [ Red, Green, Blue, Yellow, Pink, White ]


pickColor index =
    Array.get index colors |> Maybe.withDefault Red


randColor : Random.Generator Color
randColor =
    Random.uniform Red [ Green, Blue, Yellow, Pink, White ]


type Pick
    = Pick Color Color Color Color


roll : Random.Generator Pick
roll =
    Random.map4 Pick randColor randColor randColor randColor


pickToRow (Pick a b c d) =
    Row a b c d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
