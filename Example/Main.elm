module Main exposing (..)

import Calendar exposing (DayContent, Model, initCalendarModel, view)
import Dict
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
    { calendarModel : Calendar.Model Msg
    }


init : ( Model, Cmd Msg )
init =
    ( Model (initCalendarModel (text "default")), Cmd.none )


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Calendar.view model.calendarModel
