module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Calendar


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = Sub.none
        }


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    1


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Html Msg
view =
    h1 [] [ text ]
