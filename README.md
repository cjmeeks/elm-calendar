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

Subscriptions - The drag and drop requires subscriptions
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CMsg <| Calendar.subscriptions model.calendarModel
```

The Calendar.initCalendarModel initializes a CalendarModel for you then you can pass in the html data to be displayed
```elm

testData : List ( ( Int, Int, Int ), MyData )
testData =
    [ ( ( 2018, 6, 1 ), MyData "Test Click" 0 )
    , ( ( 2018, 6, 20 ), MyData "Test Click" 0 )
    ]

init...
( cModel, cCmd ) =
    Calendar.initCalendarModel

```

This maps the testData into a tuple of a provided Type CalendarDate and Html with the function viewData as the html generator
```elm

viewData : CalendarDate -> MyData -> Html Msg
viewData key data =
    text data.text

data =
    List.map
        (\( ( a, b, c ), data ) ->
            ( CalendarDate ( a, b, c ), viewData (CalendarDate ( a, b, c )) data )
        )
        testCase


```
Sets the initial content of the calendar with our data
```elm
calendarModel =
    setDayContent data cModel
```

To handle custom messages in the update we first see if the message is a CustomMsg(This is a CalendarMsg) and call update on that if not we update the calendar as it is calendar msg that you do not need to handle.
```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CMsg cMsg ->
            let
                ( updatedModel, cmds ) =
                    case cMsg of
                        CustomMsg customMsg ->
                            update customMsg { model | calendarModel = updatedCalendar }

                        _ ->
                          let
                            ( updatedCalendar, cCmd ) =
                              Calendar.update cMsg model.calendarModel
                          in
                            { model | calendarModel = updatedCalendar } ! []
            in
                ( updatedModel, Cmd.batch [ Cmd.map CMsg cCmd, cmds ] )
```


## TODO

* if custom day header then have a default header for days without content
  * consider just having a formatter or function
* Look into how to move day content forward and backward a month
  * Get feedback if this would be a good feature to add or to leave it to the dev to make

## Example
![See it in action!](https://i.gyazo.com/93b1460787732710eb21e3e2a4cc96cd.gif)