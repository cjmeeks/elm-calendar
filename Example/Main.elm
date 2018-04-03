module Main exposing (..)

import Calendar exposing (..)
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
        , subscriptions = subscriptions
        }


type alias Model =
    { calendarModel : Calendar.CalendarModel
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CMsg <| Calendar.subscriptions model.calendarModel


init : ( Model, Cmd Msg )
init =
    let
        ( cModel, cCmd ) =
            initCalendarModel 

    in
        ( Model <| setDayContent testCase cModel, Cmd.map CMsg cCmd )


type Msg
    = CMsg CalendarMsg
    | ItemHasMoved CalendarDate CalendarDate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CMsg cMsg ->
            let
                toFromDates = 
                    Calendar.catchToAndFromDates cMsg model.calendarModel

                ( updatedCalendar, cCmd ) =
                    Calendar.update cMsg model.calendarModel
                
                (updatedModel, cmds) =
                    case toFromDates of
                        Just dates ->
                            update (ItemHasMoved dates.from dates.to) {model | calendarModel = updatedCalendar }
                            
                    
                        Nothing -> {model | calendarModel = updatedCalendar } ! []
                            
            in
                ( updatedModel, Cmd.batch [Cmd.map CMsg cCmd, cmds] )
                
        ItemHasMoved from to ->
            let
                temp = Debug.log "dates: " (from,to)
            in
                model ! []


view : Model -> Html Msg
view model =
    Html.map CMsg <| Calendar.view model.calendarModel



testCustomForwardButton =
    div [] [text "Forward"]

testCustomBackButton = 
    div [] [text "Back"]

testCase : List (CalendarDate, Html CalendarMsg)
testCase =
    [ (CalendarDate (2018, 4, 1) ,div [] [ text "hello" ])
    , (CalendarDate (2018, 4, 20) ,div [] [ text "hello3" ])
    , (CalendarDate (2018, 4, 2) ,div [] [ text "hello4" ])
    , (CalendarDate (2018, 4, 14) ,div [] [ text "hello5" ])
    , (CalendarDate (2018, 4, 23) ,div [] [ text "hello2" ])
    , (CalendarDate (2018, 4, 10) ,div [] [ text "hello6" ])
    -- , (CalendarDate (2018, 4, 10) ,div [] [ text "hello7" ])
    ]
