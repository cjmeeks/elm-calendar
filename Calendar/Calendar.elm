module Calendar
    exposing
        ( initCalendarModel
        , update
        , view
        , setDayContent
        , subscriptions
        , turnOnCustomButtons
        , setCustomForwardButton
        , setCustomBackButton
        , addDayContent
        , deleteDayContent
        , catchToAndFromDates
        , setHeaderHeight
        , setSidebarWidth
        )

{-| This library is for a drag and drop calendar


# views

@docs view


# update

@docs update


# subscriptions

@docs subscriptions


# model

@docs initCalendarModel


# functions

Inits and setters for customizing the html

@docs setDayContent, setCustomForwardButton, setCustomBackButton, turnOnCustomButtons, addDayContent, deleteDayContent
@docs catchToAndFromDates, setHeaderHeight, setSidebarWidth

-}

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (..)
import Task exposing (..)
import Time.Date as TDate exposing (..)
import Mouse exposing (Position)
import Window exposing (..)
import Calendar.Internal exposing (..)
import Calendar.Types exposing (..)
import Calendar.Styles exposing (..)


--TODO
-- make config have default window sizing or have custom div size -> stretch
-- have a way for user to see which content was moved to update their stuff


initConfig : Config a
initConfig =
    { customDayHeader = False
    , customButtons = False
    , customStuff = initCustomStuff
    , toggleDragging = True
    , customHeaderHeight = 0
    , customSidebarWidth = 0
    }


{-|

    turnOnCustomButtons
-}
turnOnCustomButtons : CalendarModel a -> CalendarModel a
turnOnCustomButtons model =
    let
        config =
            model.config

        newConfig =
            { config | customButtons = True }
    in
        { model | config = newConfig }


{-|

    turnOffCustomButtons
-}
turnOffCustomButtons : CalendarModel a -> CalendarModel a
turnOffCustomButtons model =
    let
        config =
            model.config

        newConfig =
            { config | customButtons = False }
    in
        { model | config = newConfig }


{-|

    turnOnCustomButtons
-}
turnOnDragging : CalendarModel a -> CalendarModel a
turnOnDragging model =
    let
        config =
            model.config

        newConfig =
            { config | toggleDragging = True }
    in
        { model | config = newConfig }


{-|

    turnOffCustomButtons
-}
turnOffDragging : CalendarModel a -> CalendarModel a
turnOffDragging model =
    let
        config =
            model.config

        newConfig =
            { config | toggleDragging = False }
    in
        { model | config = newConfig }


{-|

    turnOffDefaultHeader
-}
turnOffDefaultHeader : CalendarModel a -> CalendarModel a
turnOffDefaultHeader model =
    let
        config =
            model.config

        newConfig =
            { config | customDayHeader = True }
    in
        { model | config = newConfig }


{-|

    setHeaderHeight
-}
setHeaderHeight : Int -> CalendarModel a -> CalendarModel a
setHeaderHeight height model =
    let
        config =
            model.config

        newConfig =
            { config | customHeaderHeight = height }
    in
        { model | config = newConfig }


{-|

    setSidebarWidth
-}
setSidebarWidth : Int -> CalendarModel a -> CalendarModel a
setSidebarWidth width model =
    let
        config =
            model.config

        newConfig =
            { config | customSidebarWidth = width }
    in
        { model | config = newConfig }


initCustomStuff : CustomStuff a
initCustomStuff =
    { forwardButton = div [] []
    , backButton = div [] []
    , headerFormat = ""
    }


{-|

    setCustomForwardButton
-}
setCustomForwardButton : Html a -> CalendarModel a -> CalendarModel a
setCustomForwardButton btn model =
    let
        config =
            model.config

        custom =
            config.customStuff

        newButtons =
            { custom | forwardButton = btn }

        newConfig =
            { config | customButtons = True, customStuff = newButtons }
    in
        { model | config = newConfig }


{-|

    setCustomBackButton
-}
setCustomBackButton : Html a -> CalendarModel a -> CalendarModel a
setCustomBackButton btn model =
    let
        config =
            model.config

        custom =
            config.customStuff

        newButtons =
            { custom | backButton = btn }

        newConfig =
            { config | customButtons = True, customStuff = newButtons }
    in
        { model | config = newConfig }


{-|

    The daycontent
    pass in msg type of your html
-}
initCalendarModel : ( CalendarModel a, Cmd (CalendarMsg a) )
initCalendarModel =
    ( CalendarModel Dict.empty Nothing 0 0 Nothing Nothing (Window.Size 0 0) initConfig, Cmd.batch [ Date.now |> Task.perform RecieveDate, Task.perform (\x -> Resize x) Window.size ] )


{-|

    subscriptions : Calendar Subscriptions
-}
subscriptions : CalendarModel a -> Sub (CalendarMsg a)
subscriptions model =
    let
        dragSub =
            case model.drag of
                Nothing ->
                    Sub.none

                Just _ ->
                    if model.config.toggleDragging then
                        Sub.map Drags <| Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
                    else
                        Sub.none
    in
        Sub.batch [ Window.resizes Resize, dragSub ]



{-
   initial function to get the daterange
-}


initWithDayContent : List (DayContent a) -> CalendarModel a
initWithDayContent list =
    let
        ( min, max ) =
            getMinAndMaxDate <| List.map .theDate list

        dates =
            datesInRange min max

        datesList =
            List.map initDayContentFromDate dates

        combined =
            combineDateRangeWithListOfDayContent datesList list []
    in
        CalendarModel Dict.empty Nothing 0 0 Nothing Nothing (Window.Size 0 0) initConfig


{-| Displays the Calendar

    Calendar.view

-}
view : CalendarModel a -> Html (CalendarMsg a)
view model =
    let
        dates =
            datesInRange (date 2018 1 1) (date 2018 1 31)

        getMonth =
            Dict.get ( model.currentYear, model.currentMonth ) model.months

        monthContent =
            case getMonth of
                Just item ->
                    viewMonth model item

                Nothing ->
                    [ text "There was an Error" ]

        listOfMonths =
            List.map Tuple.second <|
                Dict.toList model.months

        firstDateInContent =
            case List.head listOfMonths of
                Just m ->
                    ( m.year, m.month )

                Nothing ->
                    ( 2000, 1 )

        ( curYear, curMonth, curDay ) =
            case model.currentDate of
                Just d ->
                    TDate.toTuple d

                Nothing ->
                    ( 2000, 1, 1 )

        forwardButton =
            if model.config.customButtons then
                model.config.customStuff.forwardButton
            else
                div [ headerButton ] [ text ">>" ]

        backButton =
            if model.config.customButtons then
                model.config.customStuff.backButton
            else
                div [ headerButton ] [ text "<<" ]

        heightOfDiv =
            model.size.height - model.config.customHeaderHeight

        subHeader =
            div [ subHeaderGrid, gridAccessSpanCol 2 7 1 ]
                subHeaders
    in
        div [ class "calendar-container" ]
            [ div [ calendarGrid heightOfDiv ]
                [ div
                    [ gridAccessSpanCol 1 7 1
                    , headerGrid
                    , class "calendar-header-container"
                    ]
                    [ div
                        [ onClick MonthBackward
                        , gridAccess 1 1
                        , class "calendar-header-back-button"
                        ]
                        [ Html.map CustomMsg backButton ]
                    , h1
                        [ calendarHeader
                        , gridAccess 1 2
                        , class "calendar-header-title"
                        ]
                        [ text <| monthToString model.currentMonth ]
                    , div
                        [ onClick MonthForward
                        , gridAccess 1 3
                        , class "calendar-header-forward-button"
                        ]
                        [ Html.map CustomMsg forwardButton ]
                    ]
                , subHeader
                , div [ calendarDayGrid <| round (0.9 * (toFloat heightOfDiv)), gridAccessSpanCol 3 7 1 ] monthContent
                ]
            ]


{-| updates the Calendar

    Calendar.update

-}
update : CalendarMsg a -> CalendarModel a -> ( CalendarModel a, Cmd (CalendarMsg a) )
update msg model =
    case msg of
        RecieveDate rDate ->
            let
                ( monthInt, yearInt ) =
                    ( getMonthInt (Date.month rDate), Date.year rDate )

                newMonths =
                    case Dict.get ( yearInt, monthInt ) model.months of
                        Just _ ->
                            model.months

                        Nothing ->
                            Dict.insert ( yearInt, monthInt ) (insertDumbyMonth yearInt monthInt) model.months
            in
                { model
                    | currentDate = Just (date (Date.year rDate) monthInt (Date.day rDate))
                    , currentMonth = monthInt
                    , currentYear = yearInt
                    , months = newMonths
                }
                    ! []

        MonthForward ->
            let
                ( newMonth, newYear ) =
                    if model.currentMonth == 12 then
                        ( 1, model.currentYear + 1 )
                    else
                        ( model.currentMonth + 1, model.currentYear )

                updatedContent =
                    case Dict.get ( newYear, newMonth ) model.months of
                        Just m ->
                            model.months

                        Nothing ->
                            Dict.insert ( newYear, newMonth ) (insertDumbyMonth newYear newMonth) model.months
            in
                { model | currentMonth = newMonth, currentYear = newYear, months = updatedContent } ! []

        MonthBackward ->
            let
                ( newMonth, newYear ) =
                    if model.currentMonth == 1 then
                        ( 12, model.currentYear - 1 )
                    else
                        ( model.currentMonth - 1, model.currentYear )

                updatedContent =
                    case Dict.get ( newYear, newMonth ) model.months of
                        Just m ->
                            model.months

                        Nothing ->
                            Dict.insert ( newYear, newMonth ) (insertDumbyMonth newYear newMonth) model.months
            in
                { model | currentMonth = newMonth, currentYear = newYear, months = updatedContent } ! []

        Drags msg_ ->
            updateDrags msg_ model

        Resize newSize ->
            { model | size = newSize } ! []

        CustomMsg m ->
            model ! []

        DoNothing ->
            model ! []


{-| Catch the from date and to date of the moved content for use by the user
-}
catchToAndFromDates : CalendarMsg a -> CalendarModel a -> Maybe MovedDates
catchToAndFromDates msg model =
    case msg of
        Drags dMsg ->
            case dMsg of
                DragEnd pos ->
                    let
                        ( toDate, fromDate ) =
                            getFromAndToDates pos model
                    in
                        case ( toDate, fromDate ) of
                            ( Just toCalendarDate, Just fromCalendarDate ) ->
                                Just <| MovedDates toCalendarDate fromCalendarDate

                            _ ->
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



-- Date Stuff


listOfMonthInts : List Int
listOfMonthInts =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]


{-| Takes a list of day content and sets that as the calendar content
-}
setDayContent : List ( CalendarDate, Html a ) -> CalendarModel a -> CalendarModel a
setDayContent days model =
    let
        ( min, max ) =
            getMinAndMaxDate <| List.map (\( CalendarDate ( year, month, d ), h ) -> TDate.date year month d) days

        dates =
            datesInRange min max

        datesList =
            List.map initDayContentFromDate dates

        daysList =
            List.sortWith (\t1 t2 -> TDate.compare t1.theDate t2.theDate) <| List.map (\( CalendarDate ( year, month, d ), c ) -> DayContent 0 0 c (TDate.date year month d)) days

        combined =
            combineDateRangeWithListOfDayContent datesList daysList []

        daysToInternalMonths =
            groupDayContentByMonth combined

        gridStuff =
            List.map
                (\x ->
                    { x
                        | days =
                            Dict.fromList <|
                                List.map (\y -> ( ( TDate.year y.theDate, TDate.month y.theDate, TDate.day y.theDate ), y )) <|
                                    getMonthGridFromDates <|
                                        List.map Tuple.second <|
                                            Dict.toList x.days
                    }
                )
                daysToInternalMonths

        monthDict =
            Dict.fromList <| List.map (\x -> ( ( x.year, x.month ), x )) gridStuff
    in
        { model | months = monthDict }


{-| Takes a tuple of (year, month, date) and Html content corresponding to that date
-}
addDayContent : ( Int, Int, Int ) -> Html a -> CalendarModel a -> CalendarModel a
addDayContent ( year, month, day ) content model =
    let
        getMonth =
            case Dict.get ( year, month ) model.months of
                Just m ->
                    m

                Nothing ->
                    InternalMonth 0 0 Dict.empty

        newMonthContent =
            case Dict.get ( year, month, day ) getMonth.days of
                Just d ->
                    Dict.insert ( year, month, day ) { d | content = content } getMonth.days

                Nothing ->
                    Dict.insert ( year, month, day ) (DayContent 0 0 content (TDate.fromTuple ( year, month, day ))) getMonth.days

        newMonth =
            { getMonth | days = newMonthContent }

        newModelContent =
            Dict.insert ( year, month ) newMonth model.months
    in
        { model | months = newModelContent }


{-| Takes a tuple of (year, month, date) to be deleted
-}
deleteDayContent : ( Int, Int, Int ) -> CalendarModel a -> CalendarModel a
deleteDayContent ( year, month, day ) model =
    let
        newMonth =
            case Dict.get ( year, month ) model.months of
                Just m ->
                    let
                        newDays =
                            Dict.remove ( year, month, day ) m.days
                    in
                        { m | days = newDays }

                Nothing ->
                    InternalMonth 0 0 Dict.empty

        newModelContent =
            Dict.insert ( year, month ) newMonth model.months
    in
        { model | months = newModelContent }
