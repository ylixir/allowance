module Bar exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Date exposing (..)
import Set exposing (..)
import Json.Decode as Json


--model
--update


currencyToString : Int -> Int -> String
currencyToString precision amount =
    (if amount < 0 then
        "-"
     else
        ""
    )
        ++ String.join "."
            [ toString <| (abs amount) // 10 ^ precision
            , String.padLeft 2 '0' <| toString <| (abs amount) % 10 ^ precision
            ]


stringToCurrency : Int -> Int -> String -> Int
stringToCurrency precision default amount =
    amount
        |> String.toFloat
        |> (Result.withDefault <| (toFloat default) / (toFloat <| 10 ^ precision))
        |> (*) (toFloat (10 ^ precision))
        |> truncate


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


addTransaction : Int -> List Int -> List Int
addTransaction t l =
    case t of
        0 ->
            l

        _ ->
            t :: l


updateBar : BarMsg -> NativeBar -> Maybe NativeBar
updateBar msg bar =
    case msg of
        BeginEdit ->
            Just { bar | edit = Just freshBarEdit }

        FinishEdit ->
            Just { bar | edit = Nothing }

        RemoveBar ->
            Nothing

        AddGroup group ->
            case (defaultFreshBarEdit bar.edit).newGroup of
                "" ->
                    Just bar

                g ->
                    Just { bar | edit = Just freshBarEdit, groups = Set.insert g bar.groups }

        UpdateEditGroup g ->
            Just { bar | edit = Just <| BarEdit g }

        RemoveGroup g ->
            Just { bar | groups = Set.remove g bar.groups }

        UpdateTransactionEdit amount ->
            Just
                { bar
                    | transactions =
                        Transactions
                            amount
                            bar.transactions.list
                }

        AddTransaction t ->
            let
                barTrans =
                    bar.transactions
            in
                Just { bar | transactions = { barTrans | list = addTransaction (stringToCurrency bar.precision 0 t) barTrans.list } }

        SubtractTransaction t ->
            let
                barTrans =
                    bar.transactions
            in
                case stringToCurrency bar.precision 0 t of
                    0 ->
                        Just bar

                    n ->
                        Just { bar | transactions = { barTrans | list = -n :: barTrans.list } }

        UpdateName name ->
            Just { bar | name = name }

        UpdateStart start ->
            Just { bar | startDate = fromString start }

        UpdateAmount amount ->
            Just
                { bar | amountPerTime = stringToCurrency bar.amountPerTime bar.precision amount }

        UpdateRate rate ->
            Just { bar | timePerAmount = stringToTime rate }

        UpdateInterval interval ->
            Just { bar | interval = stringToTime interval }



--view


timeToString : Time -> String
timeToString time =
    String.join " " <|
        List.filter (\s -> not (String.startsWith "0" s))
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


onEnter : a -> Attribute a
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"
    in
        on "keyup" (Json.andThen isEnter keyCode)


renderGroupEdit : (BarMsg -> Msg) -> String -> Set String -> Html Msg
renderGroupEdit route new groups =
    div []
        ([ div [] [ text "groups" ]
         , input
            [ placeholder "new group"
            , value new
            , onInput (\n -> route <| UpdateEditGroup n)
            , onEnter (route <| AddGroup new)
            ]
            []
         , button [ onClick (route <| AddGroup new) ] [ text "+" ]
         ]
            ++ (groups
                    |> Set.toList
                    |> List.map (\g -> div [] [ text g, button [ onClick (route (RemoveGroup g)) ] [ text "♲" ] ])
               )
        )


renderSettings : (BarMsg -> Msg) -> NativeBar -> Html Msg
renderSettings route bar =
    let
        dateResultToString : Result String Date -> Result String String
        dateResultToString date =
            case date of
                Ok date ->
                    Ok (dateToString date)

                Err msg ->
                    Err ""
    in
        div
            [ style
                [ ( "background-color", "white" )
                ]
            ]
            [ div [ onInput (\n -> route (UpdateName n)) ] [ text "name", input [ value bar.name ] [] ]
            , renderGroupEdit route (barEditGroup (defaultFreshBarEdit bar.edit)) bar.groups
            , div [ onInput (\n -> route (UpdateStart n)) ]
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
                    [ defaultValue <| currencyToString bar.precision bar.amountPerTime
                    , onInput (\n -> route (UpdateAmount n))
                    ]
                    []
                , text (bar.currency ++ " per ")
                , input
                    [ placeholder "1d 1h 3m 1s"
                    , defaultValue (timeToString bar.timePerAmount)
                    , onInput (\n -> route (UpdateRate n))
                    ]
                    []
                ]
            , div []
                [ text "interval"
                , input
                    [ defaultValue (timeToString bar.interval)
                    , onInput (\n -> route (UpdateInterval n))
                    ]
                    []
                ]
            , button [ onClick (route FinishEdit) ] [ text "✓" ]
            , button [ onClick (route RemoveBar) ] [ text "♲" ]
            ]


renderBar : (BarMsg -> Msg) -> NativeBar -> Maybe Time -> Html Msg
renderBar route bar time =
    let
        ( start, diff ) =
            case bar.startDate of
                Ok date ->
                    ( toString date, (Maybe.withDefault 0 time) - (toTime date) )

                Err message ->
                    ( message, 0 )

        interval =
            if (bar.interval == 0) && (bar.amountPerTime /= 0) then
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
        div [ classes [ "bar__container", "border" ] ]
            [ div [ class "bar__title" ] [ text <| bar.name ++ " since " ++ start ]
            , div []
                [ div
                    [ classes [ "bar__value", "strong" ]
                    ]
                    [ (appliedTime / bar.timePerAmount * toFloat bar.amountPerTime)
                        |> (\n ->
                                if (isNaN n) || (isInfinite n) then
                                    0
                                else
                                    n
                           )
                        |> floor
                        |> (+) (List.sum bar.transactions.list)
                        |> currencyToString bar.precision
                        |> (\n -> n ++ " USD")
                        |> text
                    , div
                        [ class "bar__timer"
                        , style [ ( "width", (toString (100 * (diff - appliedTime) / interval)) ++ "%" ) ]
                        ]
                        []
                    ]
                ]
            , div
                [ class "bar__controls"
                ]
                [ div [ classes [ "button" ], onClick (route BeginEdit) ] [ text "⚙" ]
                , div [ classes [ "button", "close-right" ], onClick (route <| AddTransaction bar.transactions.edit) ] [ text "+" ]
                , input
                    [ classes [ "input", "close-left", "close-right" ]
                    , value bar.transactions.edit
                    , type_ "number"
                    , placeholder "Add/Remove Money"
                    , onInput (\n -> route <| UpdateTransactionEdit n)
                    ]
                    []
                , div [ classes [ "button", "close-left" ], onClick (route <| SubtractTransaction bar.transactions.edit) ] [ text "-" ]
                ]
            ]


viewBar : (BarMsg -> Msg) -> NativeBar -> Maybe Time -> Html Msg
viewBar route bar time =
    case bar.edit of
        Just _ ->
            renderSettings route bar

        Nothing ->
            renderBar route bar time
