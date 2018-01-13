module Main exposing (main)

{-| The entrypoint for our documentation
@docs main
-}

import Html exposing (..)
import Http
import Json.Decode as Decode
import Elm.Documentation as Docs


{-| -}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| -}
type alias Model =
    Maybe (List Docs.Documentation)


{-| -}
init : ( Model, Cmd Msg )
init =
    Nothing ! [ requestDocJson ]


{-| -}
type Msg
    = DocsJson (Result Http.Error (List Docs.Documentation))


{-| -}
requestDocJson : Cmd Msg
requestDocJson =
    Http.send DocsJson <| Http.get "documentation.json" (Decode.list Docs.decoder)


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DocsJson (Ok docs) ->
            Just docs ! []

        _ ->
            Nothing ! []


{-| -}
subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


{-| -}
viewModule : Docs.Documentation -> Html Msg
viewModule docs =
    div [] [ text docs.name ]


{-| -}
view : Model -> Html Msg
view model =
    div [] <| List.map viewModule (Maybe.withDefault [] model)
