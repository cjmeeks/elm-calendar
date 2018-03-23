module Calendar exposing (CalendarMsg, DayContent, Model, initCalendarModel, update, view, dateToString, setDayContent)

{-| This library is for a drag and drop calendar


# views

@docs view


# update

@docs update


# model

@docs Model
@docs DayContent


# functions

@docs initCalendarModel
@docs dateToString
@docs setDayContent


# types

@docs CalendarMsg

-}

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Time.Date as TDate exposing (..)
import Set
import List.Extra


{-|

    The model
    pass in msg type of your html
    content is a dictionary of key: (Year, Month) to Value : Internal Month
-}
type alias Model =
    { newContent : Dict.Dict ( Int, Int ) InternalMonth
    , currentDate : Maybe TDate.Date
    , currentMonth : Int
    , currentYear : Int
    }


{-| calendar msg
-}
type CalendarMsg
    = RecieveDate Date.Date
    | MonthForward
    | MonthBackward
    | DoNothing


{-|

    The daycontent
    pass in msg type of your html
-}
type alias DayContent =
    { dayIndex : Int
    , weekIndex : Int
    , content : Html CalendarMsg
    , theDate : TDate.Date
    }



{-
   key of days is (year,month,day)
-}


type alias InternalMonth =
    { month : Int
    , year : Int
    , days : Dict.Dict ( Int, Int, Int ) DayContent
    }


{-|

    The daycontent
    pass in msg type of your html
-}
initCalendarModel : Html CalendarMsg -> ( Model, Cmd CalendarMsg )
initCalendarModel defaultHtml =
    ( Model Dict.empty Nothing 0 0, Date.now |> Task.perform RecieveDate )



{-
   initial function to get the daterange
-}


initWithDayContent : List DayContent -> Model
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

        temp =
            Debug.log "list" combined
    in
        Model Dict.empty Nothing 0 0


initDayContentFromDate : Date -> DayContent
initDayContentFromDate d =
    DayContent 0 0 (div [] []) d


{-| Displays the Calendar

    Calendar.view

-}
view : Model -> Html CalendarMsg
view model =
    let
        dates =
            datesInRange (date 2018 1 1) (date 2018 1 31)

        listOfMonths =
            List.map Tuple.second <| Dict.toList model.newContent

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

        getMonth =
            Dict.get ( model.currentYear, model.currentMonth ) model.newContent

        monthContent =
            case getMonth of
                Just item ->
                    let
                        temp =
                            Debug.log "days" item.days
                    in
                        viewMonth item

                Nothing ->
                    [ text "There was an Error" ]
    in
        div [ calendarGrid ] <|
            List.append
                [ div
                    [ gridAccessSpanCol 1 7 1, headerGrid ]
                    [ button [ onClick MonthBackward, gridAccess 1 1 ] [ text "<<" ]
                    , h1 [ calendarHeader, gridAccess 1 2 ]
                        [ text "CALENDAR!!" ]
                    , button [ onClick MonthForward, gridAccess 1 3 ] [ text ">>" ]
                    ]
                ]
                monthContent


viewMonth : InternalMonth -> List (Html CalendarMsg)
viewMonth month =
    List.map
        (\( x, dayContent ) ->
            div [ gridAccess dayContent.weekIndex dayContent.dayIndex, gridItem ]
                [ h1 [] [ text <| dateToString dayContent.theDate ]
                , dayContent.content
                ]
        )
    <|
        Dict.toList month.days



-- <|
-- viewMonthFromList indexed


viewMonthFromList : List ( ( Int, Int ), Date ) -> List (Html CalendarMsg)
viewMonthFromList list =
    List.map (\( ( x, y ), d ) -> div [ gridAccess y x, gridItem ] [ text <| TDate.toISO8601 d ]) list


{-| updates the Calendar

    Calendar.update

-}
update : CalendarMsg -> Model -> ( Model, Cmd CalendarMsg )
update msg model =
    case msg of
        RecieveDate rDate ->
            let
                monthInt =
                    getMonthInt (Date.month rDate)
            in
                ( { model
                    | currentDate = Just (date (Date.year rDate) monthInt (Date.day rDate))
                    , currentMonth = monthInt
                    , currentYear = Date.year rDate
                  }
                , Cmd.none
                )

        MonthForward ->
            let
                ( newMonth, newYear ) =
                    if model.currentMonth == 12 then
                        ( 1, model.currentYear + 1 )
                    else
                        ( model.currentMonth + 1, model.currentYear )

                updatedContent =
                    case Dict.get ( newMonth, newYear ) model.newContent of
                        Just m ->
                            model.newContent

                        Nothing ->
                            Dict.insert ( newYear, newMonth ) (insertDumbyMonth newYear newMonth) model.newContent
            in
                ( { model | currentMonth = newMonth, currentYear = newYear, newContent = updatedContent }, Cmd.none )

        MonthBackward ->
            let
                ( newMonth, newYear ) =
                    if model.currentMonth == 1 then
                        ( 12, model.currentYear - 1 )
                    else
                        ( model.currentMonth - 1, model.currentYear )

                updatedContent =
                    case Dict.get ( newMonth, newYear ) model.newContent of
                        Just m ->
                            model.newContent

                        Nothing ->
                            Dict.insert ( newYear, newMonth ) (insertDumbyMonth newYear newMonth) model.newContent
            in
                ( { model | currentMonth = newMonth, currentYear = newYear, newContent = updatedContent }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


{-|

    converts a date to a string
-}
dateToString :
    Date
    -> String --maybe make internal rich type for this string
dateToString date =
    String.join " " [ toString <| TDate.month date, toString <| TDate.day date, toString <| TDate.year date ]


getMinAndMaxDate : List Date -> ( Date, Date )
getMinAndMaxDate dates =
    let
        newDates =
            List.map toTuple dates

        maxDate =
            if List.length newDates >= 2 then
                case List.maximum newDates of
                    Just ( y, m, d ) ->
                        date y m d

                    Nothing ->
                        date 2018 12 31
            else
                date 2018 12 31

        minDate =
            if List.length newDates >= 2 then
                case List.minimum newDates of
                    Just ( y, m, d ) ->
                        date y m d

                    Nothing ->
                        date 2018 1 1
            else
                date 2018 1 1

        minMonthDate =
            date (year minDate) (month minDate) 1

        maxMonthDate =
            date (year maxDate) (month maxDate) (daysInMonth (year maxDate) (month maxDate))
    in
        ( minMonthDate, maxMonthDate )


combineDateRangeWithListOfDayContent : List DayContent -> List DayContent -> List DayContent -> List DayContent
combineDateRangeWithListOfDayContent dateRange list acc =
    case ( dateRange, list ) of
        ( d :: dli, m :: mli ) ->
            let
                dDate =
                    TDate.toTuple d.theDate

                mDate =
                    TDate.toTuple m.theDate
            in
                if dDate == mDate then
                    combineDateRangeWithListOfDayContent dli mli <| List.append acc [ m ]
                else
                    combineDateRangeWithListOfDayContent dli (m :: mli) <| List.append acc [ d ]

        ( [], m :: mli ) ->
            combineDateRangeWithListOfDayContent [] mli <| List.append acc [ m ]

        ( d :: dli, [] ) ->
            combineDateRangeWithListOfDayContent dli [] <| List.append acc [ d ]

        ( [], [] ) ->
            acc



{- STYLES -}


gridAccess : Int -> Int -> Attribute msg
gridAccess row col =
    style
        [ ( "grid-row", toString row )
        , ( "grid-column", toString col )
        ]


gridItem : Attribute msg
gridItem =
    style
        [ ( "border-style", "solid" )
        , ( "border-width", "2px" )
        ]


gridAccessSpanRow : Int -> Int -> Int -> Attribute msg
gridAccessSpanRow row span col =
    style
        [ ( "grid-row", toString row ++ " / " ++ "span " ++ toString span )
        , ( "grid-column", toString col )
        ]


gridAccessSpanCol : Int -> Int -> Int -> Attribute msg
gridAccessSpanCol row span col =
    style
        [ ( "grid-row", toString row )
        , ( "grid-column", toString col ++ " / " ++ "span " ++ toString span )
        ]


calendarGrid : Attribute msg
calendarGrid =
    style
        [ ( "display", "grid" )
        , ( "height", "100%" )
        , ( "width", "100%" )
        , ( "grid-template-rows", "5% 19% 19% 19% 19% 19%" )
        , ( "grid-template-columns", "14.2% 14.2% 14.2% 14.2% 14.2% 14.2%" )
        ]


headerGrid : Attribute msg
headerGrid =
    style
        [ ( "display", "grid" )
        , ( "height", "100%" )
        , ( "width", "100%" )
        , ( "grid-template-columns", "10% 80% 10%" )
        ]


calendarHeader : Attribute msg
calendarHeader =
    style
        [ ( "margin", "0px" )
        , ( "text-align", "center" )
        ]



-- Date Stuff


listOfMonthInts : List Int
listOfMonthInts =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]


{-| Takes a list of day content and sets that as the calendar content
-}
setDayContent : List DayContent -> Model -> Model
setDayContent days model =
    let
        ( min, max ) =
            getMinAndMaxDate <| List.map .theDate days

        dates =
            datesInRange min max

        datesList =
            List.map initDayContentFromDate dates

        combined =
            combineDateRangeWithListOfDayContent datesList days []

        daysToInternalMonths =
            groupDayContentByMonth combined

        gridStuff =
            List.map (\x -> { x | days = Dict.fromList <| List.map (\y -> ( ( TDate.year y.theDate, TDate.month y.theDate, TDate.day y.theDate ), y )) <| getMonthGridFromDates <| List.map Tuple.second <| Dict.toList x.days }) daysToInternalMonths

        monthDict =
            Dict.fromList <| List.map (\x -> ( ( x.year, x.month ), x )) gridStuff
    in
        { model | newContent = monthDict }



--TODO make this do dict int (internal month)
-- setters
--addDayContent adds html at that key uses dict so anything there before will be deleted
--(year,month,day)


insertDumbyMonth : Int -> Int -> InternalMonth
insertDumbyMonth year month =
    let
        dates =
            datesInRange (date year month 1) (date year month (daysInMonth year month))

        dayContent =
            Dict.fromList <| List.map (\x -> ( ( TDate.year x.theDate, TDate.month x.theDate, TDate.day x.theDate ), x )) <| getMonthGridFromDates <| List.map initDayContentFromDate dates
    in
        InternalMonth month year dayContent


addDayContent : ( Int, Int, Int ) -> Html CalendarMsg -> Model -> Model
addDayContent ( year, month, day ) content model =
    let
        getMonth =
            case Dict.get ( year, month ) model.newContent of
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
            Dict.insert ( year, month ) newMonth model.newContent
    in
        { model | newContent = newModelContent }



--DeleteDayContent


deleteDayContent : ( Int, Int, Int ) -> Model -> Model
deleteDayContent ( year, month, day ) model =
    let
        newMonth =
            case Dict.get ( year, month ) model.newContent of
                Just m ->
                    let
                        newDays =
                            Dict.remove ( year, month, day ) m.days
                    in
                        { m | days = newDays }

                Nothing ->
                    InternalMonth 0 0 Dict.empty

        newModelContent =
            Dict.insert ( year, month ) newMonth model.newContent
    in
        { model | newContent = newModelContent }


groupDayContentByMonth : List DayContent -> List InternalMonth
groupDayContentByMonth content =
    let
        sortedByYear =
            List.Extra.groupWhile (\x y -> TDate.year x.theDate == TDate.year y.theDate) content

        sortedByMonth =
            List.map (\x -> List.Extra.groupWhile (\y z -> TDate.month y.theDate == TDate.month z.theDate) x) sortedByYear

        toInternalMonthList =
            List.map (\x -> List.map listToInternalMonth x) sortedByMonth
    in
        List.concat toInternalMonthList


listToInternalMonth : List DayContent -> InternalMonth
listToInternalMonth content =
    let
        contentDict =
            Dict.fromList <| List.map (\x -> ( (TDate.toTuple x.theDate), x )) content
    in
        case List.head content of
            Just item ->
                (InternalMonth (TDate.month item.theDate) (TDate.year item.theDate) contentDict)

            Nothing ->
                InternalMonth 2000 1 Dict.empty



--List Date -> ((x,y), Date) map that
-- Recurcive function
-- gets a list of daycontent which will generally be given as a month at a time and updates their day and week indexes for the grid
-- make this take in internal month


getMonthGridFromDates : List DayContent -> List DayContent
getMonthGridFromDates dates =
    let
        getGridXY list row acc =
            case list of
                d :: li ->
                    let
                        xPos =
                            dayToGridxPosition (TDate.weekday d.theDate)
                    in
                        if xPos == 99 then
                            getGridXY li row acc
                        else if xPos == 7 then
                            getGridXY li (row + 1) (List.append acc [ { d | dayIndex = xPos, weekIndex = row } ])
                        else
                            getGridXY li row (List.append acc [ { d | dayIndex = xPos, weekIndex = row } ])

                [] ->
                    acc
    in
        getGridXY dates 2 []


updateContent : Model -> Model
updateContent model =
    let
        listOfMonths =
            Dict.toList model.newContent

        newListOfMonths =
            List.map
                (\( key, x ) ->
                    ( key, updateInternalMonthGrid x )
                )
                listOfMonths
    in
        { model | newContent = Dict.fromList newListOfMonths }


updateInternalMonthGrid : InternalMonth -> InternalMonth
updateInternalMonthGrid month =
    let
        days =
            List.map Tuple.second <| Dict.toList month.days

        newDays =
            Dict.fromList <| List.map (\x -> ( TDate.toTuple x.theDate, x )) <| getMonthGridFromDates days
    in
        { month | days = newDays }


dayToGridxPosition : Weekday -> Int
dayToGridxPosition weekd =
    case weekd of
        Mon ->
            2

        Tue ->
            3

        Wed ->
            4

        Thu ->
            5

        Fri ->
            6

        Sat ->
            7

        Sun ->
            1



--------------------------------------------------------------------------
{- date functions adapted from goilluminate/elm-fancy-daterangepicker
   https://github.com/GoIlluminate/elm-fancy-daterangepicker/blob/master/src/DateRangePicker/Date.elm
   these are adapted to use elm-community/elm-time funtions and date type
-}
--------------------------------------------------------------------------
-- getMonthRange : Date -> Date -> List Date
-- getMonthRange min max =
--     datesInRange min (subDay max)


{-| A function that gets all the dates between two dates (inclusive).
-}
datesInRange : Date -> Date -> List Date
datesInRange min max =
    let
        go x acc =
            let
                y =
                    subDay x
            in
                if toTuple y == toTuple min then
                    y :: acc
                else
                    go y (y :: acc)
    in
        go (addDay max) []


{-| A function that takes a Date and returns the date representing the first of that month.
-}
startOfMonth : Date -> Date
startOfMonth dateIn =
    date (year dateIn) (month dateIn) 1


{-| A function that takes a Date and returns the date representing the end of that month.
-}
endOfMonth : Date -> Date
endOfMonth dateIn =
    let
        y =
            year dateIn

        m =
            month dateIn

        d =
            case TDate.daysInMonth y m of
                Just i ->
                    i

                Nothing ->
                    0
    in
        date y m d


{-| A function that subtracts 1 day from the given date and returns it.
-}
subDay : Date -> Date
subDay dateIn =
    let
        monthNum =
            month dateIn

        yearNum =
            year dateIn

        dayNum =
            day dateIn - 1

        pred =
            predMonth monthNum

        predYear =
            if pred == 12 then
                yearNum - 1
            else
                yearNum

        predDay =
            case TDate.daysInMonth predYear pred of
                Just i ->
                    i

                Nothing ->
                    0
    in
        if dayNum < 1 then
            date predYear pred predDay
        else
            date yearNum monthNum dayNum


{-| A function that adds 1 day to the given date and returns it.
-}
addDay : Date -> Date
addDay dateIn =
    let
        monthNum =
            month dateIn

        yearNum =
            year dateIn

        dim =
            case TDate.daysInMonth yearNum monthNum of
                Just i ->
                    i

                Nothing ->
                    0

        dayNum =
            day dateIn + 1

        succ =
            succMonth monthNum

        succYear =
            if succ == 1 then
                yearNum + 1
            else
                yearNum
    in
        if dayNum > dim then
            date succYear succ 1
        else
            date yearNum monthNum dayNum


getMonthInt : Date.Month -> Int
getMonthInt d =
    case d of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


monthFromInt : Int -> Date.Month
monthFromInt d =
    case d of
        1 ->
            Date.Jan

        2 ->
            Date.Feb

        3 ->
            Date.Mar

        4 ->
            Date.Apr

        5 ->
            Date.May

        6 ->
            Date.Jun

        7 ->
            Date.Jul

        8 ->
            Date.Aug

        9 ->
            Date.Sep

        10 ->
            Date.Oct

        11 ->
            Date.Nov

        12 ->
            Date.Dec

        _ ->
            Date.Jan


{-| An opaque function to determine whether a given year is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear y =
    y % 400 == 0 || y % 100 /= 0 && y % 4 == 0


{-| An opaque function returning the next month.
-}
succMonth : Int -> Int
succMonth month =
    month
        |> flip rem 12
        |> (+) 1


{-| An opaque function returning the previous month.
-}
predMonth : Int -> Int
predMonth month =
    let
        prev =
            (month - 1)
                |> flip rem 12
    in
        if prev == 0 then
            12
        else
            prev


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        1 ->
            31

        2 ->
            if isLeapYear year then
                29
            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            31
