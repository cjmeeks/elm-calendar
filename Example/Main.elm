module Main exposing (..)

import Calendar exposing (CalendarMsg, DayContent, Model, initCalendarModel, update, view)
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
    { calendarModel : Calendar.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( cModel, cCmd ) =
            initCalendarModel (text "default")
    in
    ( Model cModel, Cmd.map CMsg cCmd )


type Msg
    = CMsg CalendarMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CMsg cMsg ->
            let
                ( updatedModel, cCmd ) =
                    Calendar.update cMsg model.calendarModel
            in
            ( { model | calendarModel = updatedModel }, Cmd.map CMsg cCmd )


view : Model -> Html Msg
view model =
    Html.map CMsg <| Calendar.view model.calendarModel
