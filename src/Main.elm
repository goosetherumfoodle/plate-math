module Main exposing (..)

import Html exposing (Html, text, div, h1, h3, button, hr, select, option, label)
import Html.Attributes exposing (src, style, class, disabled, defaultValue, value, name, selected)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator, generate, int)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, rx, ry, x, y)
import List exposing (concat, head, tail, sum, map, drop, range)
import Bootstrap.CDN as CDN


-- todo: make weight the combined of the pairs? in plate list
-- todo: re-organize
---- MODEL ----


type alias Model =
    { target : Int
    , total : Int
    , outcome : Maybe Bool
    , plates : List PlatePair
    , maxTarget : Int
    }


type alias PlatePair =
    { leftPlate : PlateSvgAttrs
    , rightPlate : PlateSvgAttrs
    , weight : PlateWeight
    }


type alias PlateSvgAttrs =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


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


barWeight : Int
barWeight =
    45


type PlateWeight
    = FourtyFive
    | ThirtyFive
    | TwentyFive
    | Ten
    | Five
    | TwoPointFive


allPlates : List PlateWeight
allPlates =
    [ FourtyFive
    , ThirtyFive
    , TwentyFive
    , Ten
    , Five
    , TwoPointFive
    ]



---- UPDATE ----


type Msg
    = AddPlates PlateWeight
    | Reset
    | Target Int
    | Test
    | Undo
    | MaxTarget String


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

        MaxTarget newTarget ->
            ( { model | maxTarget = selectedOrDefaultTarget newTarget }, Cmd.none )


selectedOrDefaultTarget : String -> Int
selectedOrDefaultTarget selected =
    case String.toInt selected of
        Ok int ->
            int

        Err _ ->
            weightWithTwo45s maxPlatePairs


maxPlatePairs =
    6


roomForMorePlates : List PlatePair -> Bool
roomForMorePlates pairs =
    if List.length pairs < maxPlatePairs then
        True
    else
        False


addPlates : PlateWeight -> Model -> Model
addPlates weight model =
    case model.outcome of
        Nothing ->
            addSvgPlates weight model

        Just _ ->
            model


addSvgPlates : PlateWeight -> Model -> Model
addSvgPlates weight model =
    if roomForMorePlates model.plates then
        let
            newPlates =
                buildPlatePair (carPlates model.plates) weight
        in
            { model | plates = consPlates newPlates model.plates }
    else
        model


carPlates : List PlatePair -> Maybe PlatePair
carPlates =
    head


consPlates : PlatePair -> List PlatePair -> List PlatePair
consPlates =
    (::)


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
        FourtyFive ->
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



---- VIEW ----


maxTargets =
    map weightWithTwo45s (range 1 maxPlatePairs)


weightWithTwo45s numPlates =
    numPlates * round (toNumber FourtyFive * 2) + barWeight


maxTargetSelect current =
    select
        [ defaultValue "max"
        , onInput MaxTarget
        ]
    <|
        placeholderOption
            :: map maxTargetOption maxTargets


placeholderOption =
    option [ disabled True, selected True ] [ text "Up To" ]


maxTargetOption max =
    option [] [ text <| toString max ]


view : Model -> Html Msg
view model =
    div [ class "container text-center" ]
        [ CDN.stylesheet
        , h1 [] [ text "Plate Math Quiz" ]
        , barbellSvg model.plates
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
                , maxTargetSelect model.maxTarget
                ]
            ]
        , plateRackDiv model.outcome allPlates
        ]


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


newTarget : Int -> Cmd Msg
newTarget maxTarget =
    generate Target <| targetGenerator maxTarget


targetGenerator : Int -> Generator Int
targetGenerator maxTarget =
    Random.map (divisibleBy 5) <| int barWeight maxTarget


divisibleBy : Int -> Int -> Int
divisibleBy x y =
    y - (y % x) + x



---- SVG WEIGHTS ------


barbellSvg : List PlatePair -> Html Msg
barbellSvg plates =
    div []
        [ svg
            [ viewBox "0 0 400 120"
            ]
          <|
            concat
                [ emptyBar
                , List.map (plateRect << .leftPlate) plates
                , List.map (plateRect << .rightPlate) plates
                ]
        ]


plateRect : PlateSvgAttrs -> Svg Msg
plateRect attrs =
    rect
        [ x <| toString attrs.x
        , y <| toString attrs.y
        , width <| toString attrs.width
        , height <| toString attrs.height
        , rx "2"
        , ry "3"
        ]
        []


buildPlatePair : Maybe PlatePair -> PlateWeight -> PlatePair
buildPlatePair prevPlates weight =
    case prevPlates of
        Nothing ->
            { leftPlate = buildLeftPlate Nothing weight
            , rightPlate = buildRightPlate Nothing weight
            , weight = weight
            }

        Just pair ->
            { leftPlate = buildLeftPlate (Just pair.leftPlate) weight
            , rightPlate = buildRightPlate (Just pair.rightPlate) weight
            , weight = weight
            }


plateBuffer : Float
plateBuffer =
    0


buildLeftPlate : Maybe PlateSvgAttrs -> PlateWeight -> PlateSvgAttrs
buildLeftPlate prevPlate weight =
    case prevPlate of
        Nothing ->
            { x = 70 - calcWidth weight
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }

        Just plate ->
            { x = plate.x - plateBuffer - calcWidth weight
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }


buildRightPlate : Maybe PlateSvgAttrs -> PlateWeight -> PlateSvgAttrs
buildRightPlate prevPlate weight =
    case prevPlate of
        Nothing ->
            { x = 340
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }

        Just plate ->
            { x = plate.x + plate.width + plateBuffer
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }


calcHeight : PlateWeight -> Float
calcHeight weight =
    case weight of
        FourtyFive ->
            120

        ThirtyFive ->
            100

        TwentyFive ->
            70

        Ten ->
            50

        Five ->
            30

        TwoPointFive ->
            28


calcY : PlateWeight -> Float
calcY weight =
    case weight of
        FourtyFive ->
            0

        ThirtyFive ->
            10

        TwentyFive ->
            25

        Ten ->
            35

        Five ->
            45

        TwoPointFive ->
            46


calcWidth : PlateWeight -> Float
calcWidth weight =
    case weight of
        FourtyFive ->
            10

        ThirtyFive ->
            10

        TwentyFive ->
            9

        Ten ->
            6

        Five ->
            5

        TwoPointFive ->
            2


emptyBar : List (Svg Msg)
emptyBar =
    [ rect [ x "0", y "58", width "400", height "5", rx "1", ry "2" ] []
    , rect [ x "330", y "52", width "10", height "18", rx "1", ry "2" ] []
    , rect [ x "70", y "52", width "10", height "18", rx "1", ry "2" ] []
    , rect [ x "329", y "56", width "70", height "9", rx "1", ry "2" ] []
    , rect [ x "0", y "56", width "71", height "9", rx "1", ry "2" ] []
    ]
