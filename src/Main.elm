port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)
import Task exposing (perform)
import Bar exposing (..)
import Time exposing (Time, hour, second)
import Types exposing (..)
import Dict exposing (..)
import Set exposing (..)


main : Program (Maybe (Model JsonBar)) (Model NativeBar) Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- MODEL


init : Maybe (Model JsonBar) -> ( Model NativeBar, Cmd Msg )
init savedModel =
    ( jsonToModel <| Maybe.withDefault (modelToJson emptyModel) savedModel, Task.perform Tick Time.now )



-- UPDATE


port saveModel : Model JsonBar -> Cmd msg


updateWithStorage : Msg -> Model NativeBar -> ( Model NativeBar, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        case msg of
            Tick time ->
                ( newModel, cmds )

            _ ->
                ( newModel
                , Cmd.batch
                    [ cmds
                    , (saveModel (modelToJson newModel))
                    ]
                )


update : Msg -> Model NativeBar -> ( Model NativeBar, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            { model | time = Just time } ! []

        AddBar ->
            { model | bars = (emptyBar model.uuid) :: model.bars, uuid = model.uuid + 1 } ! []

        UpdateBar barId msg ->
            { model
                | bars =
                    List.filterMap
                        (\b ->
                            if b.id == barId then
                                updateBar msg b
                            else
                                Just b
                        )
                        model.bars
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model NativeBar -> Sub Msg
subscriptions model =
    --Sub.none
    Time.every second Tick



-- VIEW


addBarToList : NativeBar -> Maybe (List NativeBar) -> Maybe (List NativeBar)
addBarToList newBar oldBars =
    case oldBars of
        Nothing ->
            Just [ newBar ]

        Just list ->
            Just (newBar :: list)


updateGroupsWithBar : NativeBar -> Dict String (List NativeBar) -> Dict String (List NativeBar)
updateGroupsWithBar bar groups =
    case Set.isEmpty bar.groups of
        True ->
            Dict.update "Orphans" (addBarToList bar) groups

        False ->
            Set.foldl (\g d -> Dict.update g (addBarToList bar) d) groups bar.groups


topLevelStyle : Attribute msg
topLevelStyle =
    style [ ( "margin", "0.5em" ) ]


view : Model NativeBar -> Html Msg
view model =
    div [] <|
        div [ topLevelStyle ] [ div [ classes [ "button" ], onClick AddBar ] [ text "Make new..." ] ]
            :: Dict.foldl
                (viewGroup model.time)
                []
                (List.foldl
                    updateGroupsWithBar
                    Dict.empty
                    model.bars
                )


viewBars : List NativeBar -> Maybe Time -> List (Html Msg)
viewBars bars time =
    List.map
        (\b ->
            div [ topLevelStyle ]
                [ viewBar (UpdateBar b.id) b time
                ]
        )
        bars


viewGroup : Maybe Time -> String -> List NativeBar -> List (Html Msg) -> List (Html Msg)
viewGroup time name bars previous =
    div [ topLevelStyle ]
        [ div [ classes [ "group__container", "border" ] ]
            ((div [ classes [ "group__name", "heading" ] ] [ text name ])
                :: viewBars bars time
            )
        ]
        :: previous
