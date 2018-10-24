module Types exposing (Bar, BarEdit, BarMsg(..), Indexed(..), JsonBar, JsonStartDate, Model, Msg(..), NativeBar, NativeStartDate, Transactions, barEditGroup, barToJson, classes, dateToJson, defaultFreshBarEdit, emptyBar, emptyModel, emptyTransactions, freshBarEdit, indexedList, jsonToBar, jsonToDate, jsonToModel, listIndices, modelToJson, testModel)

import Html exposing (..)
import Html.Attributes exposing (..)
import Iso8601
import Set exposing (..)
import Time exposing (Posix)
import TimeUnits exposing (hour, second)


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
    Result String Posix


dateToJson : NativeStartDate -> JsonStartDate
dateToJson dateResult =
    case dateResult of
        Ok date ->
            { ok = True
            , string = date |> Time.posixToMillis |> String.fromInt
            }

        Err string ->
            { ok = False
            , string = string
            }


jsonToDate : JsonStartDate -> NativeStartDate
jsonToDate json =
    case json.ok of
        True ->
            case json.string |> String.toInt of
                Just val ->
                    Result.Ok <| Time.millisToPosix val

                Nothing ->
                    Result.Err ("Couldn't convert " ++ json.string ++ " to integer.")

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


type alias Bar =
    { name : String
    , id : Int
    , timePerAmount : Posix
    , interval : Posix
    , amountPerTime : Int
    , currency : String
    , precision : Int
    , edit : Maybe BarEdit
    , transactions : Transactions
    }


type alias SlottedBar a b =
    { startDate : a
    , groups : b
    , bar : Bar
    }


type alias NativeBar =
    SlottedBar NativeStartDate (Set String)


type alias JsonBar =
    SlottedBar JsonStartDate (List String)


emptyBar : Int -> NativeBar
emptyBar id =
    { startDate = (Result.Err "uninitialized")
    , groups = Set.empty
    , bar = Bar "" id 0 0 0 "USD" 2 (Just freshBarEdit) emptyTransactions
    }


barToJson : NativeBar -> JsonBar
barToJson bar =
    { startDate = (dateToJson bar.startDate)
    , groups = (Set.toList bar.groups)
    , bar = bar.bar
    }


jsonToBar : JsonBar -> NativeBar
jsonToBar bar =
    { startDate = (jsonToDate bar.startDate)
    , groups = (Set.fromList bar.groups)
    , bar = bar.bar
    }


type alias Model a =
    { time : Maybe Posix
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
            ( \bar -> { bar | groups = Set.singleton "Aidan" } )
            [ { bar = Bar "Spending" 0 (604800 * second) 0 100 "USD" 2 Nothing emptyTransactions
              , startDate = Iso8601.toTime "2017-03-11"
              }
            , { bar = Bar "Saving" 1 (604800 * second) (7 * 24 * hour) 100 "USD" 2 Nothing emptyTransactions
              , startDate = Iso8601.toTime "2017-03-11"
              }
            , { bar = Bar "Giving" 2 (604800 * second) (24 * hour) 100 "USD" 2 Nothing emptyTransactions
              , startDate = Iso8601.toTime "2017-03-11"
              }
            ]
            ++ List.map (\bar -> {bar | groups = Set.singleton "Val"})
                [ { bar = Bar "Spending" 3 (604800 * second) 0 200 "USD" 2 Nothing emptyTransactions
                  , startDate = Iso8601.toTime "2017-03-11"
                  }
                , { bar = Bar "Saving" 4 (604800 * second) (604800 * second) 200 "USD" 2 Nothing emptyTransactions
                  , startDate = Iso8601.toTime "2017-03-11"
                  }
                , { bar = Bar "Giving" 5 (604800 * second) (24 * hour) 200 "USD" 2 Nothing emptyTransactions
                  , startDate = Iso8601.toTime "2017-03-11"
                  }
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
    = Tick Posix
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


classes : List String -> Html.Attribute msg
classes l =
    l
        |> List.map (\c -> ( c, True ))
        |> classList
