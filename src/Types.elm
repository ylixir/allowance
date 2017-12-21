module Types exposing (..)

import Time exposing (Time, hour, second)
import Date exposing (Date)
import Set exposing (..)


type Indexed a
    = Indexed a Int


listIndices : List a -> List Int
listIndices l =
    List.length l
        |> (-) 1
        |> List.range 0


indexedList : List a -> List (Indexed a)
indexedList l =
    List.map2 Indexed l (listIndices l)


type alias JsonStartDate =
    { ok : Bool
    , string : String
    }


type alias NativeStartDate =
    Result String Date


dateToJson : NativeStartDate -> JsonStartDate
dateToJson dateResult =
    case dateResult of
        Ok date ->
            { ok = True
            , string = date |> Date.toTime |> toString
            }

        Err string ->
            { ok = False
            , string = string
            }


jsonToDate : JsonStartDate -> NativeStartDate
jsonToDate json =
    case json.ok of
        True ->
            case json.string |> String.toFloat of
                Ok val ->
                    Result.Ok <| Date.fromTime val

                Err msg ->
                    Result.Err msg

        False ->
            Result.Err json.string


type alias BarEdit =
    { newGroup : String
    }


defaultFreshBarEdit : Maybe BarEdit -> BarEdit
defaultFreshBarEdit =
    Maybe.withDefault freshBarEdit


freshBarEdit : BarEdit
freshBarEdit =
    BarEdit ""


barEditGroup : BarEdit -> String
barEditGroup { newGroup } =
    newGroup


type alias Transactions =
    { edit : String
    , list : List Int
    }


emptyTransactions : Transactions
emptyTransactions =
    Transactions "" []


type alias Bar a b =
    { name : String
    , id : Int
    , startDate : a
    , timePerAmount : Time
    , interval : Time
    , amountPerTime : Int
    , currency : String
    , precision : Int
    , edit : Maybe BarEdit
    , transactions : Transactions
    , groups : b
    }


type alias NativeBar =
    Bar NativeStartDate (Set String)


type alias JsonBar =
    Bar JsonStartDate (List String)


emptyBar : Int -> Bar NativeStartDate (Set String)
emptyBar id =
    Bar "" id (Result.Err "uninitialized") 0 0 0 "USD" 2 (Just freshBarEdit) emptyTransactions Set.empty


barToJson : NativeBar -> JsonBar
barToJson bar =
    { bar | groups = (Set.toList bar.groups), startDate = dateToJson bar.startDate }


jsonToBar : JsonBar -> NativeBar
jsonToBar bar =
    { bar | groups = Set.fromList bar.groups, startDate = jsonToDate bar.startDate }


type alias Model a =
    { time : Maybe Time
    , bars : List a
    , version : Int
    , uuid : Int
    }


emptyModel : Model NativeBar
emptyModel =
    Model Nothing [] 0 0


testModel : Model NativeBar
testModel =
    Model Nothing
        (List.map
            (\bar -> bar <| Set.singleton "Aidan")
            [ Bar "Spending" 0 (Date.fromString "2017-03-11") (604800 * second) 0 100 "USD" 2 Nothing emptyTransactions
            , Bar "Saving" 1 (Date.fromString "2017-03-11") (604800 * second) (7 * 24 * hour) 100 "USD" 2 Nothing emptyTransactions
            , Bar "Giving" 2 (Date.fromString "2017-03-11") (604800 * second) (24 * hour) 100 "USD" 2 Nothing emptyTransactions
            ]
            ++ List.map (\bar -> bar <| Set.singleton "Val")
                [ Bar "Spending" 3 (Date.fromString "2017-03-11") (604800 * second) 0 200 "USD" 2 Nothing emptyTransactions
                , Bar "Saving" 4 (Date.fromString "2017-03-11") (604800 * second) (604800 * second) 200 "USD" 2 Nothing emptyTransactions
                , Bar "Giving" 5 (Date.fromString "2017-03-11") (604800 * second) (24 * hour) 200 "USD" 2 Nothing emptyTransactions
                ]
        )
        0
        6


modelToJson : Model NativeBar -> Model JsonBar
modelToJson model =
    { model | bars = List.map barToJson model.bars }


jsonToModel : Model JsonBar -> Model NativeBar
jsonToModel model =
    { model | bars = List.map jsonToBar model.bars }


type Msg
    = Tick Time
    | AddBar
    | UpdateBar Int BarMsg


type BarMsg
    = BeginEdit
    | FinishEdit
    | RemoveBar
    | AddGroup String
    | UpdateEditGroup String
    | RemoveGroup String
    | UpdateTransactionEdit String
    | AddTransaction String
    | SubtractTransaction String
    | UpdateName String
    | UpdateStart String
    | UpdateAmount String
    | UpdateRate String
    | UpdateInterval String
