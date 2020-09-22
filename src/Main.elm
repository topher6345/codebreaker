module Main exposing (Color(..), Feedback, Hint(..), Row(..), detectCorrectPosition, main, mkFeedback)

import Array exposing (Array(..))
import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe exposing (Maybe(..))
import Random
import String


type Color
    = Red
    | Blue
    | Green
    | Yellow
    | Pink
    | White
    | None


colorShow : Color -> String
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


mkColor : String -> Color
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


blankRow : Row
blankRow =
    Row None None None None


nonEmptyRow : Row -> Bool
nonEmptyRow (Row a b c d) =
    List.all ((/=) None) [ a, b, c, d ]


type RowIndex
    = First
    | Second
    | Third
    | Fourth


updateRow : Row -> RowIndex -> Color -> Row
updateRow row rowIndex color =
    case ( rowIndex, row ) of
        ( First, Row _ b c d ) ->
            Row color b c d

        ( Second, Row a _ c d ) ->
            Row a color c d

        ( Third, Row a b _ d ) ->
            Row a b color d

        ( Fourth, Row a b c _ ) ->
            Row a b c color


getFromRow : Row -> RowIndex -> Color
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


type Hint
    = CorrectColorPosition
    | CorrectColor
    | Empty


showHint : Hint -> String
showHint hint =
    case hint of
        CorrectColorPosition ->
            "◾️"

        CorrectColor ->
            "◽️"

        Empty ->
            "☐"


type alias Feedback =
    { correctColorPosition : Int
    , correctColor : Int
    , empty : Int
    }


initFeedback : Feedback
initFeedback =
    { correctColorPosition = 0
    , correctColor = 0
    , empty = 4
    }


mkFeedback : Row -> Row -> Feedback
mkFeedback actual expected =
    let
        ( corrects, list ) =
            detectCorrectPosition actual expected

        ( a, b ) =
            transpose list

        colorCount =
            detectCorrectColor a b 0
    in
    { correctColorPosition = corrects
    , correctColor = colorCount
    , empty = 4 - corrects - colorCount
    }


detectCorrectPosition : Row -> Row -> ( Int, List ( Color, Color ) )
detectCorrectPosition actual expected =
    let
        zipped =
            zipRow actual expected

        equal ( a, b ) =
            a == b

        notEqual ( a, b ) =
            a /= b
    in
    ( count equal zipped
    , List.filter notEqual zipped
    )


zipRow : Row -> Row -> List ( Color, Color )
zipRow (Row a b c d) (Row e f g h) =
    [ ( a, e ), ( b, f ), ( c, g ), ( d, h ) ]


count : (a -> Bool) -> List a -> Int
count predicate list =
    let
        apply elem int =
            if predicate elem then
                int + 1

            else
                int
    in
    List.foldr apply 0 list


transpose : List ( Color, Color ) -> ( List Color, List Color )
transpose rows =
    ( List.map Tuple.first rows, List.map Tuple.second rows )


detectCorrectColor : List Color -> List Color -> Int -> Int
detectCorrectColor expected actual counter =
    case expected of
        expectedHead :: expectedTail ->
            if List.member expectedHead actual then
                detectCorrectColor
                    (List.Extra.remove expectedHead expected)
                    (List.Extra.remove expectedHead actual)
                    (counter + 1)

            else
                detectCorrectColor expectedTail actual counter

        [] ->
            counter


type alias Guess =
    ( Feedback, Row )


initGuess : Guess
initGuess =
    ( initFeedback, blankRow )


initGuesses : Array Guess
initGuesses =
    Array.repeat 8 initGuess


updateRowColor : Array Guess -> RowIndex -> String -> Int -> Guess
updateRowColor guesses rowIndex string colIndex =
    let
        ( feedback, row ) =
            guesses
                |> Array.get colIndex
                |> Maybe.withDefault initGuess
    in
    ( feedback, updateRow row rowIndex (mkColor string) )


type alias Model =
    { currentRound : Int
    , row : Row
    , guesses : Array Guess
    , pick : Row
    , reveal : Bool
    , flash : String
    }


initialModel : Model
initialModel =
    { currentRound = 0
    , row = blankRow
    , pick = Row Red Red Red Red
    , guesses = initGuesses
    , flash = "Welcome to Codebreaker!"
    , reveal = False
    }


type Msg
    = UpdateColor RowIndex Int String
    | Roll Row
    | Submit
    | Cheat
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model
                | currentRound = 0
                , row = blankRow
                , pick = Row Red Red Red Red
                , guesses = initGuesses
                , flash = "Welcome to Codebreaker!"
                , reveal = False
              }
            , Random.generate Roll roll
            )

        UpdateColor rowIndex colIndex string ->
            ( { model
                | guesses = Array.set colIndex (updateRowColor model.guesses rowIndex string colIndex) model.guesses
              }
            , Cmd.none
            )

        Roll pick ->
            ( { model | pick = pick }, Cmd.none )

        Submit ->
            let
                index =
                    model.currentRound

                ( _, row ) =
                    model.guesses
                        |> Array.get index
                        |> Maybe.withDefault initGuess

                feedback =
                    mkFeedback row model.pick

                newGuesses =
                    Array.set index ( feedback, row ) model.guesses

                currentRound =
                    model.currentRound + 1
            in
            case ( feedback.correctColorPosition == 4, currentRound > 8 ) of
                ( True, False ) ->
                    ( { model | flash = "You win!", currentRound = currentRound, guesses = newGuesses }, Cmd.none )

                ( False, True ) ->
                    ( { model | flash = "You Lose", currentRound = currentRound, guesses = newGuesses }, Cmd.none )

                _ ->
                    ( { model | currentRound = currentRound, guesses = newGuesses }, Cmd.none )

        Cheat ->
            ( { model | reveal = True }, Cmd.none )


choice : Array Guess -> RowIndex -> Bool -> Int -> Html Msg
choice guesses rowIndex disabled colIndex =
    let
        ( _, row2 ) =
            guesses
                |> Array.get colIndex
                |> Maybe.withDefault initGuess
    in
    Html.select
        (Html.Attributes.value (getFromRow row2 rowIndex |> colorShow)
            :: (if disabled then
                    [ attribute "disabled" "true" ]

                else
                    [ onInput (UpdateColor rowIndex colIndex) ]
               )
        )
        [ Html.option [ attribute "selected" "", attribute "value" "" ] [ text "-" ]
        , Html.option [] [ text "\u{1F7E5}" ]
        , Html.option [] [ text "\u{1F7E6}" ]
        , Html.option [] [ text "\u{1F7E9}" ]
        , Html.option [] [ text "\u{1F7E8}" ]
        , Html.option [] [ text "\u{1F7EA}" ]
        , Html.option [] [ text "⬜️" ]
        ]


hintTableList : Feedback -> List Hint
hintTableList { correctColorPosition, correctColor, empty } =
    let
        correct =
            List.repeat correctColorPosition CorrectColorPosition

        color =
            List.repeat correctColor CorrectColor

        empties =
            List.repeat empty Empty

        values =
            List.concat [ correct, color, empties ]

        length =
            List.length values
    in
    if length < 4 then
        values ++ List.repeat (4 - length) Empty

    else
        values


hintTable : Feedback -> List (Html Msg)
hintTable feedback =
    let
        array =
            feedback
                |> hintTableList
                |> Array.fromList

        mkText index =
            array
                |> Array.get index
                |> Maybe.withDefault Empty
                |> showHint
                |> text
    in
    [ table []
        [ tbody []
            [ tr [] [ td [] [ mkText 0 ], td [] [ mkText 1 ] ]
            , tr [] [ td [] [ mkText 2 ], td [] [ mkText 3 ] ]
            ]
        ]
    ]


submitable : Array Guess -> Int -> Int -> List (Html.Attribute Msg)
submitable guesses index colIndex =
    let
        ( _, row ) =
            guesses
                |> Array.get index
                |> Maybe.withDefault initGuess
    in
    if nonEmptyRow row && index == colIndex then
        [ onClick Submit ]

    else
        [ attribute "disabled" "true" ]


mkHintTable : Int -> Array Guess -> List (Html Msg)
mkHintTable index guesses =
    case Array.get index guesses of
        Just ( feedback, _ ) ->
            hintTable feedback

        Nothing ->
            []


hintsTr : Array Guess -> Html Msg
hintsTr guesses =
    let
        mkTd index =
            guesses
                |> mkHintTable index
                |> td []
    in
    tr [] (List.range 0 7 |> List.map mkTd)


guessesTds : RowIndex -> Int -> Array Guess -> List (Html Msg)
guessesTds rowIndex currentRound guesses =
    let
        mkTd index =
            td [] [ choice guesses rowIndex (currentRound /= index) index ]
    in
    List.range 0 7 |> List.map mkTd


mkSubmitRows : Array Guess -> Int -> List (Html Msg)
mkSubmitRows guesses currentRound =
    let
        mkTd index =
            td []
                [ button (submitable guesses currentRound index)
                    [ String.fromInt (index + 1) |> text ]
                ]
    in
    List.range 0 7 |> List.map mkTd


view : Model -> Html Msg
view model =
    div
        [ class "root"
        ]
        [ Html.h1 [] [ text "Codebreaker" ]
        , Html.p [] [ text model.flash ]
        , table []
            [ tbody [ class "hint" ]
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
        , if model.guesses == initialModel.guesses then
            button [ attribute "disabled" "true" ] [ text "New Game" ]

          else
            button [ onClick NewGame ] [ text "New Game" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Random.generate Roll roll
    )


randColor : Random.Generator Color
randColor =
    Random.uniform Red [ Green, Blue, Yellow, Pink, White ]


roll : Random.Generator Row
roll =
    Random.map4 Row randColor randColor randColor randColor


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
