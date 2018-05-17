# elm-calendar

```shell
elm package install cjmeeks/elm-calendar
```

## Description
An elm package for an calendar that has additional features such as:
* Reordering content
* Custom Content In each day frame
* Barebones css so that user can customize everything


## Usage

Subscriptions
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CMsg <| Calendar.subscriptions model.calendarModel
```

The Calendar.initCalendarModel initializes a CalendarModel for you then you can pass in the html data to be displayed
```elm

testCase : List ( ( Int, Int, Int ), MyData )
testCase =
    [ ( ( 2018, 6, 1 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 20 ), MyData "Test Click" 0 )
    ]
  

viewData : CalendarDate -> MyData -> Html Msg
viewData key data =
    text data.text

init...
( cModel, cCmd ) =
    Calendar.initCalendarModel

testData =
    List.map
        (\( ( a, b, c ), data ) ->
            ( CalendarDate ( a, b, c ), viewData (CalendarDate ( a, b, c )) data )
        )
        testCase

calendarModel =
    setDayContent testData cModel
```

To handle custom messages in the update

```elm
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
                            { model | calendarModel = updatedCalendar } ! []
            in
                ( updatedModel, Cmd.batch [ Cmd.map CMsg cCmd, cmds ] )
```


## TODO

* test custom sidebar
* if custom day header then have a default header for days without content
  * consider just having a formatter or function
* finish readme
* Look into how to move day content forward and backward a month
  * Get feedback if this would be a good feature to add or to leave it to the dev to make

## Example
![See it in action!](https://i.gyazo.com/93b1460787732710eb21e3e2a4cc96cd.gif)