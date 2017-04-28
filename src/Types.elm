module Types exposing (..)

import Time exposing (Time)
import Date exposing (..)


type alias JsonStartDate =
    { ok : Bool
    , string : String
    }


dateToString : Date -> String
dateToString date =
    let
        dateToMonthString : Date -> String
        dateToMonthString date =
            case (month date) of
                Date.Jan ->
                    "01"

                Date.Feb ->
                    "02"

                Date.Mar ->
                    "03"

                Date.Apr ->
                    "04"

                Date.May ->
                    "05"

                Date.Jun ->
                    "06"

                Date.Jul ->
                    "07"

                Date.Aug ->
                    "08"

                Date.Sep ->
                    "09"

                Date.Oct ->
                    "10"

                Date.Nov ->
                    "11"

                Date.Dec ->
                    "12"
    in
        (toString (year date)) ++ "-" ++ (dateToMonthString date) ++ "-" ++ (toString (day date))


dateToJson : Result String Date -> JsonStartDate
dateToJson dateResult =
    case dateResult of
        Ok date ->
            { ok = True
            , string = dateToString date
            }

        Err string ->
            { ok = False
            , string = string
            }


jsonToDate : JsonStartDate -> Result String Date
jsonToDate json =
    case json.ok of
        True ->
            Date.fromString json.string

        False ->
            Result.Err json.string


type alias JsonBar =
    { id : Int
    , name : String
    , startDate : JsonStartDate
    , timePerAmount : Time
    , interval : Time
    , amountPerTime : Int
    , currency : String
    , precision : Int
    , edit : Bool
    }


type alias Bar =
    { id : Int
    , name : String
    , startDate : Result String Date
    , timePerAmount : Time
    , interval : Time
    , amountPerTime : Int
    , currency : String
    , precision : Int
    , edit : Bool
    }


barToJson : Bar -> JsonBar
barToJson bar =
    { bar | startDate = dateToJson bar.startDate }


jsonToBar : JsonBar -> Bar
jsonToBar bar =
    { bar | startDate = jsonToDate bar.startDate }


type alias JsonGroup =
    { id : Int
    , name : String
    , bars : List JsonBar
    , uid : Int
    }


type alias Group =
    { id : Int
    , name : String
    , bars : List Bar
    , uid : Int
    }


groupToJson : Group -> JsonGroup
groupToJson group =
    { group | bars = List.map barToJson group.bars }


jsonToGroup : JsonGroup -> Group
jsonToGroup group =
    { group | bars = List.map jsonToBar group.bars }


type alias JsonModel =
    { time : Maybe Time
    , uid : Int
    , groups : List JsonGroup
    , version : Int
    }


type alias Model =
    { time : Maybe Time
    , uid : Int
    , groups : List Group
    , version : Int
    }


modelToJson : Model -> JsonModel
modelToJson model =
    { model | groups = List.map groupToJson model.groups }


jsonToModel : JsonModel -> Model
jsonToModel model =
    { model | groups = List.map jsonToGroup model.groups }


type Msg
    = Tick Time
    | FirstGroup
    | FirstBar Int
    | AddGroup Int
    | RemoveGroup Int
    | EditGroupName Int String
    | AddBar Int Int
    | RemoveBar Int Int
    | UpdateBar Int Int BarMsg


type BarMsg
    = BeginEdit
    | FinishEdit
    | UpdateName String
    | UpdateStart String
    | UpdateAmount String
    | UpdateRate String
    | UpdateInterval String
