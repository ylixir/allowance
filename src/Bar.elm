module Bar exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Date exposing (..)


--model


emptyBar : Int -> Bar
emptyBar id =
    Bar id "" (Result.Err "uninitialized") 0 0 0 "USD" 2 False



--update


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


updateBar : BarMsg -> Bar -> Bar
updateBar msg bar =
    case msg of
        BeginEdit ->
            { bar | edit = True }

        FinishEdit ->
            { bar | edit = False }

        UpdateName name ->
            { bar | name = name }

        UpdateStart start ->
            { bar | startDate = fromString start }

        UpdateAmount amount ->
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

        UpdateRate rate ->
            { bar | timePerAmount = stringToTime rate }

        UpdateInterval interval ->
            { bar | interval = stringToTime interval }



--view


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


viewSettings : (BarMsg -> Msg) -> Bar -> Html Msg
viewSettings route bar =
    case bar.edit of
        False ->
            div [] []

        True ->
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
                        [ div [ onInput (\n -> route (UpdateName n)) ] [ text "name", input [ value bar.name ] [] ]
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
                                [ defaultValue ((toString (bar.amountPerTime // 10 ^ bar.precision)) ++ "." ++ (toString (bar.amountPerTime % 10 ^ bar.precision)))
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
                        , div [] [ button [ onClick (route FinishEdit) ] [ text "✓" ] ]
                        ]
                    ]


viewBar : (BarMsg -> Msg) -> Bar -> Maybe Time -> Html Msg
viewBar route bar time =
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
        div []
            [ viewSettings route bar
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
                        , ( "width", (toString (100 * (diff - appliedTime) / interval)) ++ "%" )
                        ]
                    ]
                    []
                ]
            ]
