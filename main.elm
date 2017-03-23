module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date, fromString, toTime)
import Time exposing (Time, second, inSeconds)


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
    { name : String
    , start : Result String Date
    , growthTime : Time
    , growthInterval : Maybe Time
    , growthAmount : Float
    }



--, disbursementInterval : Int
--, currency : String


type alias BarGroup =
    { name : String, bars : List Bar }


type alias Model =
    { groups : List BarGroup
    , time : Maybe Time
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ (BarGroup "Aidan"
            [ Bar
                "Spending"
                (fromString "2017-03-18")
                (604800 * second)
                Nothing
                0.75
            ]
          )
        , (BarGroup "Val"
            [ Bar
                "Saving"
                (fromString "2017-03-12")
                (604800 * second)
                (Just (604800 * second))
                0.75
            ]
          )
        ]
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | time = Just time }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        List.map
            (viewGroup model.time)
            model.groups


viewGroup : Maybe Time -> BarGroup -> Html msg
viewGroup time group =
    div
        [ style
            [ ( "border-style", "groove" )
            , ( "margin", "0.5em" )
            ]
        ]
        ((text group.name) :: (List.map (viewBar time) group.bars))


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
            case bar.growthInterval of
                Just interval ->
                    interval

                Nothing ->
                    bar.growthTime / bar.growthAmount / 100

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
                    [ (appliedTime / bar.growthTime * bar.growthAmount * 100)
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
