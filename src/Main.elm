module Main exposing (..)

import Html
    exposing
        ( Html
        , text
        , div
        , h1
        , h3
        , button
        , hr
        , select
        , option
        , label
        , a
        , img
        )
import Html.Attributes
    exposing
        ( src
        , style
        , class
        , disabled
        , defaultValue
        , value
        , name
        , selected
        , href
        , alt
        )
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator, generate, int)
import List exposing (tail, sum, map, drop, range)
import Bootstrap.CDN as CDN
import Graphics exposing (barbellSvg, addSvgPlates)
import Types exposing (..)


-- todo: make weight the combined of the pairs? in plate list
-- todo: re-organize


barWeight : Int
barWeight =
    45


maxPlatePairs : Int
maxPlatePairs =
    6


allPlates : List PlateWeight
allPlates =
    [ FortyFive
    , ThirtyFive
    , TwentyFive
    , Ten
    , Five
    , TwoPointFive
    ]


init : ( Model, Cmd Msg )
init =
    ( { total = barWeight
      , target = 0
      , outcome = Nothing
      , plates = []
      , maxTarget = weightWithTwo45s maxPlatePairs
      }
    , newTarget <| weightWithTwo45s maxPlatePairs
    )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlates weight ->
            ( addPlates weight model, Cmd.none )

        Reset ->
            ( { model
                | total = 0
                , outcome = Nothing
                , plates = []
              }
            , newTarget model.maxTarget
            )

        Target newTarget ->
            ( { model | target = newTarget }, Cmd.none )

        Test ->
            let
                newModel =
                    sumTotal model
            in
                if newModel.total == newModel.target then
                    ( { newModel | outcome = Just True }, Cmd.none )
                else
                    ( { newModel | outcome = Just False }, Cmd.none )

        Undo ->
            case model.outcome of
                Nothing ->
                    ( { model | plates = drop 1 model.plates }, Cmd.none )

                Just _ ->
                    ( model, Cmd.none )

        MaxTarget maxTargetInput ->
            let
                newMaxTarget =
                    selectedOrDefaultTarget maxTargetInput
            in
                ( { model | maxTarget = newMaxTarget }, newTarget newMaxTarget )


selectedOrDefaultTarget : String -> Int
selectedOrDefaultTarget selected =
    case String.toInt selected of
        Ok int ->
            int

        Err _ ->
            weightWithTwo45s maxPlatePairs


addPlates : PlateWeight -> Model -> Model
addPlates weight model =
    case model.outcome of
        Nothing ->
            addSvgPlates maxPlatePairs weight model

        Just _ ->
            model


sumTotal : Model -> Model
sumTotal model =
    { model
        | total =
            sum <|
                (::) barWeight <|
                    map
                        (round << (*) 2 << toNumber << .weight)
                        model.plates
    }


toNumber : PlateWeight -> Float
toNumber weight =
    case weight of
        FortyFive ->
            45

        ThirtyFive ->
            35

        TwentyFive ->
            25

        Ten ->
            10

        Five ->
            5

        TwoPointFive ->
            2.5


newTarget : Int -> Cmd Msg
newTarget maxTarget =
    generate Target <| targetGenerator maxTarget


targetGenerator : Int -> Generator Int
targetGenerator maxTarget =
    Random.map (divisibleBy 5) <| int barWeight maxTarget


divisibleBy : Int -> Int -> Int
divisibleBy x y =
    y - (y % x) + x



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container text-center" ]
        [ CDN.stylesheet
        , githubBadge
        , h1 [] [ text "Plate Math Quiz" ]
        , barbell model.plates
        , div [ class "row" ]
            [ div [ class "col" ]
                [ text <| "Target: " ++ toString model.target ++ " lbs"
                , outcomeDiv model.outcome model.total
                ]
            , div [ class "col" ]
                [ disableableButton model.outcome
                    [ onClick Test
                    , class "btn btn-primary"
                    ]
                    (text "Check")
                , disableableButton model.outcome
                    [ onClick Undo
                    , class "btn btn-warning"
                    ]
                    (text "Undo")
                , button
                    [ onClick Reset
                    , class "btn btn-danger"
                    ]
                    [ text "Reset" ]
                , maxTargetSelect
                ]
            ]
        , plateRackDiv model.outcome allPlates
        ]


maxTargets : List Int
maxTargets =
    map weightWithTwo45s (range 1 maxPlatePairs)


weightWithTwo45s : Int -> Int
weightWithTwo45s numPlates =
    numPlates * round (toNumber FortyFive * 2) + barWeight


maxTargetSelect : Html Msg
maxTargetSelect =
    select
        [ defaultValue "max"
        , onInput MaxTarget
        ]
    <|
        placeholderOption
            :: map maxTargetOption maxTargets


placeholderOption : Html Msg
placeholderOption =
    option [ disabled True, selected True ] [ text "Up To" ]


maxTargetOption : Int -> Html Msg
maxTargetOption max =
    option [] [ text <| toString max ]


barbell : List PlatePair -> Html Msg
barbell plates =
    div [] [ barbellSvg plates ]


githubBadge : Html Msg
githubBadge =
    a [ href "https://github.com/goosetherumfoodle/plate-math" ]
        [ img
            [ style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "border", "0" )
                , ( "margin-top", "0" )
                ]
            , src "https://s3.amazonaws.com/github/ribbons/forkme_left_green_007200.png"
            , alt "Fork me on GitHub!"
            ]
            []
        ]


disableableButton : Maybe a -> List (Html.Attribute Msg) -> Html Msg -> Html Msg
disableableButton outcome style txt =
    case outcome of
        Nothing ->
            button style [ txt ]

        Just _ ->
            button (disabled True :: style) [ txt ]


outcomeDiv : Maybe Bool -> Int -> Html Msg
outcomeDiv outcome total =
    let
        displayTotal =
            text <| "Total: " ++ toString total
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


plateRackDiv : Maybe a -> List PlateWeight -> Html Msg
plateRackDiv outcome weights =
    div []
        [ hr [] []
        , h3 [] [ text "Add Plates" ]
        , div
            [ class "btn-group-lg"
            , style [ ( "role", "group" ) ]
            ]
          <|
            List.map (rackedPlateDiv outcome) weights
        ]


rackedPlateDiv : Maybe a -> PlateWeight -> Html Msg
rackedPlateDiv outcome weight =
    disableableButton outcome
        [ class "btn btn-secondary"
        , onClick <| AddPlates weight
        ]
        (text << toString <| toNumber weight)
