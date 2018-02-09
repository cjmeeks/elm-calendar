module Main exposing (..)

import Calendar
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { testHtml : Html Msg
    }


init : ( Model, Cmd Msg )
init =
    ( Model (div [] [ text "test" ]), Cmd.none )


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Calendar.view
        , model.testHtml
        ]
