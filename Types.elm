module Types exposing (..)

import Time exposing (Time)
import Date exposing (Date)


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


type alias Group =
    { id : Int
    , name : String
    , bars : List Bar
    , uid : Int
    }


type alias Model =
    { time : Maybe Time
    , uid : Int
    , groups : List Group
    }


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
