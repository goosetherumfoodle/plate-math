module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src, style, value)
import Html.Events exposing (onClick)
import Random exposing (Generator, generate, int)


---- MODEL ----


type alias Model =
    { total : Int
    , target : Int
    , outcome : Maybe Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { total = barWeight, target = 0, outcome = Nothing }, newTarget )


barWeight : Int
barWeight =
    45



---- UPDATE ----


type Msg
    = AddPlates Float
    | Reset
    | Target Int
    | Test


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlates weight ->
            ( addPlates weight model, Cmd.none )

        Reset ->
            ( { model | total = barWeight, outcome = Nothing }, newTarget )

        Target newTarget ->
            ( { model | target = newTarget }, Cmd.none )

        Test ->
            if model.total == model.target then
                ( { model | outcome = Just True }, Cmd.none )
            else
                ( { model | outcome = Just False }, Cmd.none )


addPlates weight model =
    case model.outcome of
        Nothing ->
            { model | total = model.total + (round <| weight * 2) }

        Just _ ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Plate Math" ]
        , div []
            [ div [] [ text <| "Target: " ++ toString model.target ]
            , outcomeDiv model.outcome model.total
            , button [ onClick Test ] [ text "Calculate" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , plateRackDiv [ 45, 35, 25, 10, 5, 2.5 ]
        ]


outcomeDiv : Maybe Bool -> Int -> Html Msg
outcomeDiv outcome total =
    let
        displayTotal =
            text <| "Calculated: " ++ toString total
    in
        case outcome of
            Nothing ->
                div [] [ text "" ]

            Just True ->
                div [ style [ ( "color", "green" ) ] ]
                    [ displayTotal ]

            Just False ->
                div [ style [ ( "color", "red" ) ] ]
                    [ displayTotal ]


plateRackDiv : List Float -> Html Msg
plateRackDiv weights =
    div [] <| List.map rackedPlateDiv weights


rackedPlateDiv : Float -> Html Msg
rackedPlateDiv weight =
    div
        [ style [ ( "border-style", "solid" ) ]
        , onClick <| AddPlates weight
        ]
        [ text <| toString weight ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- Domain ------


maxTarget =
    300


newTarget : Cmd Msg
newTarget =
    generate Target targetGenerator


targetGenerator : Generator Int
targetGenerator =
    Random.map (divisibleBy 5) <| int barWeight maxTarget


divisibleBy : Int -> Int -> Int
divisibleBy x y =
    y - (y % x)
