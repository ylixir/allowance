module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)
import Task exposing (perform)
import Bar exposing (..)
import Time exposing (Time, hour, second)
import Date exposing (Date, fromString, toTime, year, month, day)
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


emptyGroup : Int -> Group
emptyGroup id =
    Group id "" [] 0


emptyModel : Model
emptyModel =
    Model Nothing 0 [] 0


testModel : Model
testModel =
    Model Nothing
        2
        [ Group
            0
            "Aidan"
            [ Bar 0 "Spending" (Date.fromString "2017-03-11") (604800 * second) 0 100 "USD" 2 False
            , Bar 1 "Saving" (Date.fromString "2017-03-11") (604800 * second) (7 * 24 * hour) 100 "USD" 2 False
            , Bar 2 "Giving" (Date.fromString "2017-03-11") (604800 * second) (24 * hour) 100 "USD" 2 False
            ]
            3
        , Group
            1
            "Val"
            [ Bar 0 "Spending" (Date.fromString "2017-03-11") (604800 * second) 0 200 "USD" 2 False
            , Bar 1 "Saving" (Date.fromString "2017-03-11") (604800 * second) (604800 * second) 200 "USD" 2 False
            , Bar 2 "Giving" (Date.fromString "2017-03-11") (604800 * second) (24 * hour) 200 "USD" 2 False
            ]
            3
        ]
        0


init : ( Model, Cmd Msg )
init =
    ( testModel, Task.perform Tick Time.now )



-- UPDATE


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

        UpdateBar groupID barID msg ->
            let
                mapBar : Bar -> Bar
                mapBar bar =
                    if bar.id == barID then
                        updateBar msg bar
                    else
                        bar

                mapGroup : Group -> Group
                mapGroup group =
                    if group.id == groupID then
                        { group | bars = List.map mapBar group.bars }
                    else
                        group
            in
                { model | groups = List.map mapGroup model.groups } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    --Sub.none
    Time.every second Tick



-- VIEW


topLevelStyle : Attribute msg
topLevelStyle =
    style [ ( "margin", "0.5em" ) ]


view : Model -> Html Msg
view model =
    let
        viewGroup : Group -> Html Msg
        viewGroup group =
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
                        :: (List.map
                                (\b ->
                                    div [ topLevelStyle ]
                                        [ viewBar (UpdateBar group.id b.id) b model.time
                                        , div []
                                            [ button [ onClick (UpdateBar group.id b.id BeginEdit) ] [ text "⚙" ]
                                            , button [ onClick (AddBar group.id b.id) ] [ text "+" ]
                                            , button [ onClick (RemoveBar group.id b.id) ] [ text "-" ]
                                            ]
                                        ]
                                )
                                group.bars
                           )
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
