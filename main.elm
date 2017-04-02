module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)
import Date exposing (Date, fromString, toTime, month, day, year)
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
    , startDate : Result String Date
    , timePerAmount : Time
    , interval : Time
    , amountPerTime : Int
    , currency : String
    , precision : Int
    , edit : Bool
    }


emptyBar : Int -> Bar
emptyBar id =
    Bar id "" (Result.Err "uninitialized") 0 0 0 "USD" 2 False


timeToString : Time -> String
timeToString time =
    String.join " "
        [ time
            |> Time.inHours
            |> (\n -> n / 24)
            |> truncate
            |> (\n -> (toString n) ++ "d")
        , time
            |> Time.inHours
            |> truncate
            |> (\n -> n % 24)
            |> (\n -> (toString n) ++ "h")
        , time
            |> Time.inMinutes
            |> truncate
            |> (\n -> n % 60)
            |> (\n -> (toString n) ++ "m")
        , time
            |> Time.inSeconds
            |> truncate
            |> (\n -> n % 60)
            |> (\n -> (toString n) ++ "s")
        ]


stringToTime : String -> Time
stringToTime s =
    (\( t, m ) -> t)
        (String.foldr
            (\c ( t, m ) ->
                case c of
                    'd' ->
                        ( t, Time.hour * 24 )

                    'h' ->
                        ( t, Time.hour )

                    'm' ->
                        ( t, Time.minute )

                    's' ->
                        ( t, Time.second )

                    _ ->
                        ( t + (Result.withDefault 0 (String.toFloat (String.fromChar c))) * m, m * 10 )
            )
            ( 0, 0 )
            (String.toLower s)
        )


type alias Group =
    { id : Int
    , name : String
    , bars : List Bar
    , uid : Int
    }


emptyGroup : Int -> Group
emptyGroup id =
    Group id "" [] 0


type alias Model =
    { time : Maybe Time
    , uid : Int
    , groups : List Group
    }


init : ( Model, Cmd Msg )
init =
    ( testModel, Task.perform Tick Time.now )


emptyModel : Model
emptyModel =
    Model Nothing 0 []


testModel : Model
testModel =
    Model Nothing
        2
        [ Group
            0
            "Aidan"
            [ Bar 0 "Spending" (fromString "2017-03-11") (604800 * second) 0 100 "USD" 2 False
            , Bar 1 "Saving" (fromString "2017-03-11") (604800 * second) (7 * 24 * second) 100 "USD" 2 False
            , Bar 2 "Giving" (fromString "2017-03-11") (604800 * second) (24 * hour) 100 "USD" 2 False
            ]
            3
        , Group
            1
            "Val"
            [ Bar 0 "Spending" (fromString "2017-03-11") (604800 * second) 0 200 "USD" 2 False
            , Bar 1 "Saving" (fromString "2017-03-11") (604800 * second) (604800 * second) 200 "USD" 2 False
            , Bar 2 "Giving" (fromString "2017-03-11") (604800 * second) (24 * hour) 200 "USD" 2 False
            ]
            3
        ]



-- UPDATE


dateToText : Date -> String
dateToText date =
    (toString (year date)) ++ "-" ++ (numMonth date) ++ "-" ++ (toString (day date))


numMonth : Date -> String
numMonth date =
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


type Msg
    = Tick Time
    | FirstGroup
    | FirstBar Int
    | AddGroup Int
    | RemoveGroup Int
    | EditGroupName Int String
    | AddBar Int Int
    | RemoveBar Int Int
    | EditBar Int Int
    | FinishBarEdit Int Int
    | UpdateBarName Int Int String
    | UpdateBarStart Int Int String
    | UpdateBarAmount Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            { model | time = Just time } ! []

        FirstGroup ->
            { model | uid = model.uid + 1, groups = (emptyGroup model.uid) :: model.groups } ! []

        FirstBar id ->
            let
                updateGroup a =
                    if a.id == id then
                        { a | uid = a.uid + 1, bars = (emptyBar a.uid :: a.bars) }
                    else
                        a
            in
                { model | groups = List.map updateGroup model.groups } ! []

        AddGroup id ->
            let
                insertGroup a b =
                    if a.id == id then
                        a :: (emptyGroup model.uid) :: b
                    else
                        a :: b
            in
                { model
                    | uid = model.uid + 1
                    , groups = List.foldr insertGroup [] model.groups
                }
                    ! []

        RemoveGroup id ->
            { model | groups = List.filter (\n -> n.id /= id) model.groups } ! []

        EditGroupName id name ->
            let
                updateName a =
                    if a.id == id then
                        { a | name = name }
                    else
                        a
            in
                { model | groups = List.map updateName model.groups } ! []

        EditBar groupId barId ->
            let
                updateGroup g =
                    let
                        updateBar b =
                            if b.id == barId then
                                { b | edit = True }
                            else
                                b
                    in
                        if g.id == groupId then
                            { g | bars = List.map updateBar g.bars }
                        else
                            g
            in
                { model | groups = List.map updateGroup model.groups } ! []

        FinishBarEdit groupId barId ->
            let
                updateGroup g =
                    let
                        updateBar b =
                            if b.id == barId then
                                { b | edit = False }
                            else
                                b
                    in
                        if g.id == groupId then
                            { g | bars = List.map updateBar g.bars }
                        else
                            g
            in
                { model | groups = List.map updateGroup model.groups } ! []

        AddBar groupId barId ->
            let
                updateGroup a =
                    let
                        insertBar b c =
                            if b.id == barId then
                                b :: (emptyBar a.uid) :: c
                            else
                                b :: c
                    in
                        if a.id == groupId then
                            { a
                                | uid = a.uid + 1
                                , bars = List.foldr insertBar [] a.bars
                            }
                        else
                            a
            in
                { model | groups = List.map updateGroup model.groups }
                    ! []

        RemoveBar groupID barID ->
            let
                updateGroup a =
                    if a.id == groupID then
                        { a | bars = List.filter (\n -> n.id /= barID) a.bars }
                    else
                        a
            in
                { model | groups = List.map updateGroup model.groups } ! []

        UpdateBarName groupID barID name ->
            let
                updateGroup g =
                    let
                        updateBar b =
                            if b.id == barID then
                                { b | name = name }
                            else
                                b
                    in
                        if g.id == groupID then
                            { g | bars = List.map updateBar g.bars }
                        else
                            g
            in
                { model | groups = List.map updateGroup model.groups } ! []

        UpdateBarStart groupID barID start ->
            let
                updateGroup g =
                    let
                        updateBar b =
                            if b.id == barID then
                                { b | startDate = fromString start }
                            else
                                b
                    in
                        if g.id == groupID then
                            { g | bars = List.map updateBar g.bars }
                        else
                            g
            in
                { model | groups = List.map updateGroup model.groups } ! []

        UpdateBarAmount groupID barID amount ->
            let
                updateGroup : Group -> Group
                updateGroup group =
                    let
                        updateBar : Bar -> Bar
                        updateBar bar =
                            if bar.id == barID then
                                { bar
                                    | amountPerTime =
                                        amount
                                            |> String.toFloat
                                            |> Result.withDefault
                                                (bar.amountPerTime
                                                    |> toFloat
                                                    |> (\n -> n / toFloat (10 ^ bar.precision))
                                                )
                                            |> (*) (toFloat (10 ^ bar.precision))
                                            |> truncate
                                }
                            else
                                bar
                    in
                        if group.id == groupID then
                            { group | bars = List.map updateBar group.bars }
                        else
                            group
            in
                { model | groups = List.map updateGroup model.groups } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    --Time.every second Tick
    Sub.none



-- VIEW


topLevelStyle : Attribute msg
topLevelStyle =
    style [ ( "margin", "0.5em" ) ]


view : Model -> Html Msg
view model =
    let
        viewGroup : Group -> Html Msg
        viewGroup group =
            let
                viewBar : Bar -> Html Msg
                viewBar bar =
                    let
                        editBox =
                            case bar.edit of
                                False ->
                                    div [] []

                                True ->
                                    let
                                        dateResultToString : Result String Date -> Result String String
                                        dateResultToString date =
                                            case date of
                                                Ok date ->
                                                    Ok (dateToText date)

                                                Err msg ->
                                                    Err ""
                                    in
                                        div
                                            [ style
                                                [ ( "top", "0" )
                                                , ( "left", "0" )
                                                , ( "height", "0" )
                                                , ( "height", "100%" )
                                                , ( "width", "100%" )
                                                , ( "background-color", "rgba(0,0,0,.75)" )
                                                , ( "position", "fixed" )
                                                , ( "display", "flex" )
                                                , ( "justify-content", "center" )
                                                , ( "align-items", "center" )
                                                ]
                                            ]
                                            [ div
                                                [ style
                                                    [ ( "background-color", "white" )
                                                    ]
                                                ]
                                                [ div [ onInput (UpdateBarName group.id bar.id) ] [ text "name", input [ value bar.name ] [] ]
                                                , div [ onInput (UpdateBarStart group.id bar.id) ]
                                                    [ text "start"
                                                    , input
                                                        [ placeholder "YYYY-MM-DD"
                                                        , bar.startDate
                                                            |> dateResultToString
                                                            |> Result.withDefault ""
                                                            |> defaultValue
                                                        ]
                                                        []
                                                    ]
                                                , div []
                                                    [ text "rate"
                                                    , input
                                                        [ defaultValue ((toString (bar.amountPerTime // 10 ^ bar.precision)) ++ "." ++ (toString (bar.amountPerTime % 10 ^ bar.precision)))
                                                        , onInput (UpdateBarAmount group.id bar.id)
                                                        ]
                                                        []
                                                    , text (bar.currency ++ " per ")
                                                    , input
                                                        [ placeholder "1d 1h 3m 1s"
                                                        , defaultValue (timeToString bar.timePerAmount)
                                                        ]
                                                        []
                                                    ]
                                                , div [] [ text "interval", input [ defaultValue (timeToString bar.interval) ] [] ]
                                                , div [] [ button [ onClick (FinishBarEdit group.id bar.id) ] [ text "✓" ] ]
                                                ]
                                            ]

                        ( start, diff ) =
                            case bar.startDate of
                                Ok date ->
                                    ( toString date
                                    , case model.time of
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
                                bar.timePerAmount / toFloat bar.amountPerTime
                            else
                                bar.interval

                        appliedTime =
                            if (floor interval) == 0 then
                                diff
                            else
                                diff - toFloat ((floor diff) % (floor interval))
                    in
                        div [ topLevelStyle ]
                            [ editBox
                            , div [] [ text <| bar.name ++ " since " ++ start ]
                            , div []
                                [ div
                                    [ style
                                        [ ( "background-color", "blue" )
                                        , ( "color", "white" )
                                        , ( "width", "100%" )
                                        , ( "height", "1em" )
                                        ]
                                    ]
                                    [ (appliedTime / bar.timePerAmount * toFloat bar.amountPerTime)
                                        |> floor
                                        |> toFloat
                                        |> (\n -> n / toFloat (10 ^ bar.precision))
                                        |> toString
                                        |> (\n -> n ++ " USD")
                                        |> text
                                    ]
                                , div
                                    [ style
                                        [ ( "background-color", "green" )
                                        , ( "height", "0.2em" )
                                        , ( "width", (toString (toFloat (10 ^ bar.precision) * (diff - appliedTime) / interval)) ++ "%" )
                                        ]
                                    ]
                                    []
                                ]
                            , div []
                                [ button [ onClick (EditBar group.id bar.id) ] [ text "⚙" ]
                                , button [ onClick (AddBar group.id bar.id) ] [ text "+" ]
                                , button [ onClick (RemoveBar group.id bar.id) ] [ text "-" ]
                                ]
                            ]
            in
                div [ topLevelStyle ]
                    [ div [ style [ ( "border-style", "groove" ) ] ]
                        ((input
                            [ placeholder "Name"
                            , onInput (EditGroupName group.id)
                            , value group.name
                            , autofocus True
                            ]
                            []
                         )
                            :: div [ topLevelStyle ] [ button [ onClick (FirstBar group.id) ] [ text "+" ] ]
                            :: (List.map viewBar group.bars)
                        )
                    , div []
                        [ button [ onClick (AddGroup group.id) ] [ text "+" ]
                        , button [ onClick (RemoveGroup group.id) ] [ text "-" ]
                        ]
                    ]
    in
        div []
            (div [ topLevelStyle ] [ button [ onClick FirstGroup ] [ text "+" ] ]
                :: List.map viewGroup model.groups
            )
