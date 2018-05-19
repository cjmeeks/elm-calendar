module Simple exposing (..)

import Calendar exposing (..)
import Calendar.Types exposing (CalendarModel, CalendarDate(..), CalendarMsg(..), DragMsg(..), DayContent)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    { calendarModel : CalendarModel Msg
    , data : Dict.Dict ( Int, Int, Int ) MyData
    }


type alias MyData =
    { textStuff : String
    , count : Int
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CMsg <| Calendar.subscriptions model.calendarModel


init : ( Model, Cmd Msg )
init =
    let
        ( cModel, cCmd ) =
            initCalendarModel

        testData =
            List.map
                (\( ( a, b, c ), data ) ->
                    ( CalendarDate ( a, b, c ), viewData (CalendarDate ( a, b, c )) data )
                )
                testCase

        initModel =
            Model (setDayContent testData cModel) (Dict.fromList testCase)
    in
        ( initModel, Cmd.map CMsg cCmd )


generateDayContent : Dict.Dict ( Int, Int, Int ) MyData -> List ( CalendarDate, Html Msg )
generateDayContent myData =
    List.map
        (\( ( a, b, c ), data ) ->
            ( CalendarDate ( a, b, c ), viewData (CalendarDate ( a, b, c )) data )
        )
    <|
        Dict.toList myData


type Msg
    = CMsg (CalendarMsg Msg)
    | ItemHasMoved CalendarDate CalendarDate
    | Increment CalendarDate
    | Decrement CalendarDate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CMsg cMsg ->
            let
                ( updatedCalendar, cCmd ) =
                    Calendar.update cMsg model.calendarModel

                ( updatedModel, cmds ) =
                    case cMsg of
                        CustomMsg customMsg ->
                            update customMsg { model | calendarModel = updatedCalendar }

                        _ ->
                            case Calendar.catchToAndFromDates cMsg model.calendarModel of
                                Just dates ->
                                    update (ItemHasMoved dates.to dates.from) { model | calendarModel = updatedCalendar }

                                Nothing ->
                                    { model | calendarModel = updatedCalendar } ! []
            in
                ( updatedModel, Cmd.batch [ Cmd.map CMsg cCmd, cmds ] )

        ItemHasMoved to from ->
            updateData to from model ! []

        Increment (CalendarDate key) ->
            case Dict.get key model.data of
                Just d ->
                    let
                        updatedData =
                            Dict.insert key { d | count = d.count + 1 } model.data
                    in
                        { model
                            | data = updatedData
                            , calendarModel = setDayContent (generateDayContent updatedData) model.calendarModel
                        }
                            ! []

                Nothing ->
                    model ! []

        Decrement (CalendarDate key) ->
            case Dict.get key model.data of
                Just d ->
                    let
                        updatedData =
                            Dict.insert key { d | count = d.count - 1 } model.data
                    in
                        { model
                            | data = updatedData
                            , calendarModel = setDayContent (generateDayContent updatedData) model.calendarModel
                        }
                            ! []

                Nothing ->
                    model ! []


view : Model -> Html Msg
view model =
    Html.map CMsg <| Calendar.view model.calendarModel


viewData : CalendarDate -> MyData -> Html Msg
viewData key data =
    div []
        [ button [ onClick (Increment key) ] [ text "+" ]
        , button [ onClick (Decrement key) ] [ text "-" ]
        , text <| "Count: " ++ (toString data.count)
        ]


updateData : CalendarDate -> CalendarDate -> Model -> Model
updateData (CalendarDate to) (CalendarDate from) model =
    let
        f =
            Dict.get from model.data

        t =
            Dict.get to model.data
    in
        case ( f, t ) of
            ( Just fromData, Just toData ) ->
                let
                    newData =
                        Dict.insert to fromData <| Dict.insert from toData model.data

                    newCalendarModel =
                        setDayContent (generateDayContent newData) model.calendarModel
                in
                    { model
                        | data = newData
                        , calendarModel = newCalendarModel
                    }

            ( Just fromData, Nothing ) ->
                let
                    newData =
                        Dict.remove from <| Dict.insert to fromData model.data
                in
                    { model
                        | data = newData
                        , calendarModel = setDayContent (generateDayContent newData) model.calendarModel
                    }

            _ ->
                model


testCase : List ( ( Int, Int, Int ), MyData )
testCase =
    [ ( ( 2018, 6, 1 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 20 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 2 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 14 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 23 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 10 ), MyData "Test Click" 0 )
    ]
