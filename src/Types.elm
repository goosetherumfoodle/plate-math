module Types exposing (..)


type alias Model =
    { target : Int
    , total : Int
    , outcome : Maybe Bool
    , plates : List PlatePair
    , maxTarget : Int
    }


type Msg
    = AddPlates PlateWeight
    | Reset
    | Target Int
    | Test
    | Undo
    | MaxTarget String


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


type PlateWeight
    = FortyFive
    | ThirtyFive
    | TwentyFive
    | Ten
    | Five
    | TwoPointFive
