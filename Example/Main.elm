module Main exposing (..)

import Calendar exposing (CalendarMsg, DayContent, Model, initCalendarModel, update, view, dateToString, setDayContent)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Time.Date as Date exposing (..)


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
        ( Model <| setDayContent testCase cModel, Cmd.map CMsg cCmd )


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


testCase : List DayContent
testCase =
    [ DayContent 0 0 (div [] [ text "hello" ]) (Date.date 2018 1 1)
    , DayContent 0 0 (div [] [ text "hello2" ]) (Date.date 2018 3 20)
    ]
