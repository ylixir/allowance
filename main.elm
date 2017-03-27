module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)
import Date exposing (Date, fromString, toTime)
import Time exposing (Time, second, hour, inSeconds)
import Task exposing (perform)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Bar =
    { id : Int
    , name : String
    , start : Result String Date
    , rate : Time
    , interval : Time
    , amount : Float
    }



--, disbursementInterval : Int
--, currency : String


type alias Group =
    { id : Int
    , name : String
    , editing : Bool
    , bars : List Bar
    }


type alias Model =
    { time : Maybe Time
    , uid : Int
    , groups : List Group
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Task.perform Tick Time.now )


emptyModel : Model
emptyModel =
    Model Nothing 0 []


testModel : Model
testModel =
    Model Nothing
        8
        [ Group 0
            "Aidan"
            False
            [ Bar 1 "Spending" (fromString "2017-03-11") (604800 * second) 0 1
            , Bar 2 "Saving" (fromString "2017-03-11") (604800 * second) (7 * 24 * second) 1
            , Bar 3 "Giving" (fromString "2017-03-11") (604800 * second) (24 * hour) 1
            ]
        , Group 4
            "Val"
            False
            [ Bar 5 "Spending" (fromString "2017-03-11") (604800 * second) 0 2.0
            , Bar 6 "Saving" (fromString "2017-03-11") (604800 * second) (604800 * second) 2.0
            , Bar 7 "Giving" (fromString "2017-03-11") (604800 * second) (24 * hour) 2.0
            ]
        ]



-- UPDATE


type Msg
    = Tick Time
    | FirstGroup
    | AddGroup Int
    | ChangeGroupName Int String
    | EditGroupName Int
    | StopGroupNameEdit Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            { model | time = Just time } ! []

        FirstGroup ->
            let
                newGroup id =
                    Group id "" True []
            in
                { model | uid = model.uid + 1, groups = (newGroup model.uid) :: model.groups } ! []

        AddGroup id ->
            let
                newGroup id =
                    Group id "" True []

                insertGroup a b =
                    if a.id == id then
                        a :: newGroup model.uid :: b
                    else
                        a :: b
            in
                { model
                    | uid = model.uid + 1
                    , groups =
                        if model.groups == [] then
                            [ newGroup model.uid ]
                        else
                            List.foldr insertGroup [] model.groups
                }
                    ! []

        ChangeGroupName id name ->
            let
                updateName a =
                    if a.id == id then
                        { a | name = name }
                    else
                        a
            in
                { model | groups = List.map updateName model.groups } ! []

        EditGroupName id ->
            let
                updateGroup a =
                    if a.id == id || a.name == "" then
                        { a | editing = True }
                    else
                        { a | editing = False }
            in
                { model | groups = List.map updateGroup model.groups } ! []

        StopGroupNameEdit id ->
            let
                updateGroup a =
                    if a.id == id && a.name /= "" then
                        { a | editing = False }
                    else
                        a
            in
                { model | groups = List.map updateGroup model.groups } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


topLevelStyle : Attribute msg
topLevelStyle =
    style [ ( "margin", "0.5em" ) ]


view : Model -> Html Msg
view model =
    div []
        (div [ topLevelStyle ] [ button [ onClick FirstGroup ] [ text "+" ] ]
            :: List.map (viewGroup model.time) model.groups
        )


viewGroup : Maybe Time -> Group -> Html Msg
viewGroup time group =
    div [ topLevelStyle ]
        [ div [ style [ ( "border-style", "groove" ) ] ]
            ((if group.editing == False then
                span [ onClick (EditGroupName group.id) ] [ text group.name ]
              else
                input
                    [ placeholder "Name"
                    , onInput (ChangeGroupName group.id)
                    , onBlur (StopGroupNameEdit group.id)
                    , value group.name
                    , autofocus True
                    ]
                    []
             )
                :: (List.map (viewBar time) group.bars)
            )
        , div []
            [ button [ onClick (AddGroup group.id) ] [ text "+" ]
            , button [] [ text "-" ]
            ]
        ]


viewBar : Maybe Time -> Bar -> Html msg
viewBar time bar =
    let
        ( start, diff ) =
            case bar.start of
                Ok date ->
                    ( toString date
                    , case time of
                        Just time ->
                            time - (toTime date)

                        Nothing ->
                            0
                    )

                Err message ->
                    ( message, 0 )

        interval =
            if bar.interval == 0 then
                --make interval the time for one cent
                bar.rate / bar.amount / 100
            else
                bar.interval

        appliedTime =
            if (floor interval) == 0 then
                diff
            else
                diff - toFloat ((floor diff) % (floor interval))
    in
        div
            [ style
                [ ( "margin", "0.5em" )
                ]
            ]
            [ div [] [ text <| bar.name ++ " since " ++ start ]
            , div []
                [ div
                    [ style
                        [ ( "background-color", "blue" )
                        , ( "color", "white" )
                        , ( "width", "100%" )
                        , ( "height", "1em" )
                        ]
                    ]
                    [ (appliedTime / bar.rate * bar.amount * 100)
                        |> floor
                        |> toFloat
                        |> (\n -> n / 100)
                        |> toString
                        |> (\n -> n ++ " USD")
                        |> text
                    ]
                , div
                    [ style
                        [ ( "background-color", "green" )
                        , ( "height", "0.2em" )
                        , ( "width", (toString (100 * (diff - appliedTime) / interval)) ++ "%" )
                        ]
                    ]
                    []
                ]
            ]
