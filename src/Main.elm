port module Main exposing (..)

{-|
This is the entrypoint for the Entitlements allowance tracking application
@docs addBarToList, init, main, saveModel, subscriptions, topLevelStyle, update, updateGroupsWithBar, updateWithStorage, view, viewBars, viewGroup
-}

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)
import Task exposing (perform)
import Bar exposing (..)
import Time exposing (Time, hour, second)
import Types exposing (..)
import Dict exposing (..)
import Set exposing (..)


{-| Standard main function
    --comment
    main blerg
-}
main : Program (Maybe (Model JsonBar)) (Model NativeBar) Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- MODEL


{-| Initialize with a saved model from localhost if we have one
-}
init : Maybe (Model JsonBar) -> ( Model NativeBar, Cmd Msg )
init savedModel =
    ( jsonToModel <| Maybe.withDefault (modelToJson emptyModel) savedModel, Task.perform Tick Time.now )



-- UPDATE


{-| send the model out to js land to be saved to local storage
-}
port saveModel : Model JsonBar -> Cmd msg


{-| This is a decorator for the standard update function. It passes the model
out to a port to be saved to the local storage
-}
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


{-| Standard update functin
-}
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


{-| update the model every second so we can keep realtime track of money growth
-}
subscriptions : Model NativeBar -> Sub Msg
subscriptions model =
    --Sub.none
    Time.every second Tick



-- VIEW


{-| just push a bar onto a list, hardly deserves it's own function
-}
addBarToList : NativeBar -> Maybe (List NativeBar) -> Maybe (List NativeBar)
addBarToList newBar oldBars =
    case oldBars of
        Nothing ->
            Just [ newBar ]

        Just list ->
            Just (newBar :: list)


{-| Given a dictionary of groups, add a bar to each group in the dict that the
bar belongs to. If the bar doesn't belong to any groups, add it to the "Orphans"
group
-}
updateGroupsWithBar : NativeBar -> Dict String (List NativeBar) -> Dict String (List NativeBar)
updateGroupsWithBar bar groups =
    case Set.isEmpty bar.groups of
        True ->
            Dict.update "Orphans" (addBarToList bar) groups

        False ->
            Set.foldl (\g d -> Dict.update g (addBarToList bar) d) groups bar.groups


{-| deprecated, this should be moved to css for now, something better later
-}
topLevelStyle : Attribute msg
topLevelStyle =
    style [ ( "margin", "0.5em" ) ]


{-| standard view function
We go about this by looping through the set of bars, and putting each bar
into one or more group buckets. We then display each group.
-}
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


{-| This just takes a list of bar data structures and transforms it into a list
of rendered html bars
-}
viewBars : List NativeBar -> Maybe Time -> List (Html Msg)
viewBars bars time =
    List.map
        (\b ->
            div [ topLevelStyle ]
                [ viewBar (UpdateBar b.id) b time
                ]
        )
        bars


{-| dump out the list of bars rendered by @viewBar
-}
viewGroup : Maybe Time -> String -> List NativeBar -> List (Html Msg) -> List (Html Msg)
viewGroup time name bars previous =
    div [ topLevelStyle ]
        [ div [ classes [ "group__container", "border" ] ]
            ((div [ classes [ "group__name", "heading" ] ] [ text name ])
                :: viewBars bars time
            )
        ]
        :: previous
