module Main exposing (..)

import Html exposing (Html, text, div, h1, h3, img, button, hr)
import Html.Attributes exposing (src, style, value)
import Html.Events exposing (onClick)
import Random exposing (Generator, generate, int)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, rx, ry, x, y)
import List exposing (concat, head, tail)


---- MODEL ----


type alias Model =
    { total : Int
    , target : Int
    , outcome : Maybe Bool
    , svgPlates : List PlatePair
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
    ( { total = barWeight, target = 0, outcome = Nothing, svgPlates = [] }, newTarget )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlates weight ->
            ( addPlates weight model, Cmd.none )

        Reset ->
            ( { model
                | total = barWeight
                , outcome = Nothing
                , svgPlates = []
              }
            , newTarget
            )

        Target newTarget ->
            ( { model | target = newTarget }, Cmd.none )

        Test ->
            if model.total == model.target then
                ( { model | outcome = Just True }, Cmd.none )
            else
                ( { model | outcome = Just False }, Cmd.none )


roomForMorePlates : List PlatePair -> Bool
roomForMorePlates pairs =
    if List.length pairs <= 5 then
        True
    else
        False


addPlates : PlateWeight -> Model -> Model
addPlates weight model =
    case model.outcome of
        Nothing ->
            addSvgPlates weight (addToTotal weight model)

        Just _ ->
            model



-- todo: maybe total can be just added up from plates


addSvgPlates : PlateWeight -> Model -> Model
addSvgPlates weight model =
    if roomForMorePlates model.svgPlates then
        let
            newPlates =
                buildPlatePair (carPlates model.svgPlates) weight
        in
            { model | svgPlates = consPlates newPlates model.svgPlates }
    else
        model


carPlates : List PlatePair -> Maybe PlatePair
carPlates =
    head


consPlates : PlatePair -> List PlatePair -> List PlatePair
consPlates =
    (::)


addToTotal : PlateWeight -> Model -> Model
addToTotal weight model =
    case model.outcome of
        Nothing ->
            { model | total = model.total + (round <| (toNumber weight) * 2) }

        Just _ ->
            model


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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Plate Math" ]
        , barbellSvg model.svgPlates
        , div []
            [ div [] [ text <| "Target: " ++ toString model.target ++ " lbs" ]
            , outcomeDiv model.outcome model.total
            , button [ onClick Test ] [ text "Check" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , plateRackDiv allPlates
        ]


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


plateRackDiv : List PlateWeight -> Html Msg
plateRackDiv weights =
    div []
        [ hr [] []
        , h3 [] [ text "Click to add plates to bar" ]
        , div [] <| List.map rackedPlateDiv weights
        ]


rackedPlateDiv : PlateWeight -> Html Msg
rackedPlateDiv weight =
    button
        [ style []
        , onClick <| AddPlates weight
        ]
        [ text << toString <| toNumber weight ]



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


maxTarget : Int
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



-- todo: do we really need 'weight' in the record?! isn't it represented in the svg attrs?


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



-- todo plate svg attrs is misnamed due to presence of "weight"


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
