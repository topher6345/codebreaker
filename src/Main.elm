port module Main exposing (Color(..), Feedback, Hint(..), Row(..), detectCorrectPosition, main, mkFeedback)

import Array exposing (Array(..))
import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
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


rowToString (Row a b c d) =
    colorShow a
        ++ " "
        ++ colorShow b
        ++ " "
        ++ colorShow c
        ++ " "
        ++ colorShow d


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


feedbackToString { correctColorPosition, correctColor, empty } =
    String.fromInt correctColorPosition ++ " Bulls, " ++ String.fromInt correctColor ++ " Cows, "


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
        head :: tail ->
            if List.member head actual then
                detectCorrectColor (List.Extra.remove head expected) (List.Extra.remove head actual) (counter + 1)

            else
                detectCorrectColor tail actual counter

        [] ->
            counter


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


currentRoundDisplay currentRound =
    String.fromInt (8 - currentRound) ++ " rounds left."


type alias HistoryEntry =
    { win : Bool
    , rounds : Int
    , pick : String
    , cheat : Bool
    }


type alias Model =
    { currentRound : Int
    , row : Row
    , guesses : Array Guess
    , pick : Row
    , reveal : Bool
    , flash : String
    , history : List HistoryEntry
    , showNewGameModal : Bool
    , showClearHistoryModal : Bool
    , gameOver : Bool
    }


initialModel : Model
initialModel =
    { currentRound = 0
    , row = blankRow
    , pick = Row Red Red Red Red
    , guesses = initGuesses
    , flash = "Welcome to Codebreaker!"
    , reveal = False
    , history = []
    , showNewGameModal = False
    , showClearHistoryModal = False
    , gameOver = False
    }


type Msg
    = UpdateColor RowIndex Int String
    | Roll Row
    | Submit
    | Cheat
    | NewGame
    | ShowNewGameModal
    | DismissNewGameConfirmationModal
    | ClearHistory
    | ShowClearHistoryModal
    | DismissClearHistoryConfirmationModal


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
                , gameOver = False
                , showNewGameModal = False
              }
            , Random.generate Roll roll
            )

        DismissNewGameConfirmationModal ->
            ( { model | showNewGameModal = False }, Cmd.none )

        ShowNewGameModal ->
            ( { model | showNewGameModal = True }, Cmd.none )

        DismissClearHistoryConfirmationModal ->
            ( { model | showClearHistoryModal = False }, Cmd.none )

        ShowClearHistoryModal ->
            ( { model | showClearHistoryModal = True }, Cmd.none )

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
                    let
                        history =
                            [ { win = True, rounds = currentRound, pick = rowToString model.pick, cheat = model.reveal } ] ++ model.history
                    in
                    ( { model | flash = "You win!", gameOver = True, reveal = True, currentRound = currentRound, guesses = newGuesses, history = history }, writeHistory (encode history) )

                ( False, True ) ->
                    let
                        history =
                            [ { win = False, rounds = currentRound, pick = rowToString model.pick, cheat = model.reveal } ] ++ model.history
                    in
                    ( { model | flash = "You Lose", gameOver = True, currentRound = currentRound, guesses = newGuesses, history = history }, writeHistory (encode history) )

                _ ->
                    ( { model | flash = feedbackToString feedback ++ currentRoundDisplay currentRound, currentRound = currentRound, guesses = newGuesses }, Cmd.none )

        Cheat ->
            ( { model | reveal = True }, Cmd.none )

        ClearHistory ->
            ( { model | history = [], showClearHistoryModal = False }, writeHistory (encode []) )


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


guessesTds : RowIndex -> Model -> List (Html Msg)
guessesTds rowIndex { currentRound, guesses, gameOver } =
    let
        mkTd index =
            td [] [ choice guesses rowIndex (currentRound /= index || gameOver) index ]
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


newGameConfirmModal showNewGameModal =
    div
        [ class "new-game-confirm-modal"
        , style "display"
            (if showNewGameModal then
                "flex"

             else
                "none"
            )
        ]
        [ Html.h2 []
            [ text "Are you sure?"
            , Html.p []
                [ text "You will lose all your progress in the current game" ]
            , button
                [ onClick NewGame ]
                [ text "Yes" ]
            , button [ onClick DismissNewGameConfirmationModal ] [ text "Cancel" ]
            ]
        ]


clearHistoryConfirmModal showNewGameModal =
    div
        [ class "new-game-confirm-modal"
        , style "display"
            (if showNewGameModal then
                "flex"

             else
                "none"
            )
        ]
        [ Html.h2 []
            [ text "Are you sure?"
            , Html.p []
                [ text "This will permanently erase your game history" ]
            , button
                [ onClick ClearHistory ]
                [ text "Yes" ]
            , button [ onClick DismissClearHistoryConfirmationModal ] [ text "Cancel" ]
            ]
        ]


pickRow rowIndex model =
    tr [ class "pick" ] <|
        guessesTds rowIndex model
            ++ [ td []
                    (if model.reveal then
                        [ text <| colorShow <| getFromRow model.pick rowIndex ]

                     else
                        [ text "❓" ]
                    )
               ]


gameBoard model =
    table []
        [ tbody [ class "hint" ]
            [ hintsTr model.guesses ]
        , tbody []
            [ pickRow First model
            , pickRow Second model
            , pickRow Third model
            , pickRow Fourth model
            , tr [ class "pick" ] <|
                mkSubmitRows model.guesses model.currentRound
                    ++ [ td []
                            [ button
                                ([ onClick Cheat ]
                                    ++ (if model.reveal then
                                            [ attribute "disabled" "true" ]

                                        else
                                            []
                                       )
                                )
                                [ text "cheat" ]
                            ]
                       ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ newGameConfirmModal model.showNewGameModal
        , clearHistoryConfirmModal model.showClearHistoryModal
        , Html.h1 [] [ text "Codebreaker" ]
        , Html.p [] [ text model.flash ]
        , if model.guesses == initialModel.guesses then
            button [ attribute "disabled" "true" ] [ text "New Game" ]

          else
            button
                [ onClick
                    (if model.gameOver then
                        NewGame

                     else
                        ShowNewGameModal
                    )
                ]
                [ text "New Game" ]
        , button [ onClick ShowClearHistoryModal ] [ text "Clear History" ]
        , gameBoard model
        , Html.h2 [] [ text "Game History" ]
        , Html.ol
            [ attribute "reversed" "reversed" ]
          <|
            List.map (\historyEntry -> Html.li [] [ text <| showHistory historyEntry ]) model.history
        ]


showHistory { win, rounds, pick, cheat } =
    let
        message =
            case ( win, cheat ) of
                ( True, True ) ->
                    "You cheated in "

                ( True, False ) ->
                    "You won in "

                ( False, _ ) ->
                    "You lost in "
    in
    message ++ String.fromInt rounds ++ " rounds. " ++ pick



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


encode : List HistoryEntry -> E.Value
encode history =
    E.list
        (\h ->
            E.object
                [ ( "win", E.bool h.win )
                , ( "rounds", E.int h.rounds )
                , ( "pick", E.string h.pick )
                , ( "cheat", E.bool h.cheat )
                ]
        )
        history


decoder : D.Decoder (Array HistoryEntry)
decoder =
    D.array <|
        D.map4 HistoryEntry
            (D.field "win" D.bool)
            (D.field "rounds" D.int)
            (D.field "pick" D.string)
            (D.field "cheat" D.bool)


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok history ->
            { initialModel | history = Array.toList history }

        Err message ->
            { initialModel | flash = "Error loading history: " ++ D.errorToString message }
    , Random.generate Roll roll
    )


randColor : Random.Generator Color
randColor =
    Random.uniform Red [ Green, Blue, Yellow, Pink, White ]


roll : Random.Generator Row
roll =
    Random.map4 Row randColor randColor randColor randColor


port writeHistory : E.Value -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
