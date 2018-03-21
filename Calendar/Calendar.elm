module Calendar exposing (CalendarMsg, DayContent, Model, initCalendarModel, update, view, dateToString)

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


# types

@docs CalendarMsg

-}

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { content : Dict.Dict String DayContent
    , newContent : Dict.Dict (Int, Int) InternalMonth
    , currentDate : Maybe TDate.Date
    }


{-| calendar msg
-}
type CalendarMsg
    = RecieveDate Date.Date


{-|

    The daycontent
    pass in msg type of your html
-}
type alias DayContent =
    { dayIndex : Int
    , weekIndex : Int
    , content : Html Never
    , theDate : TDate.Date
    }


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
    ( Model Dict.empty Dict.empty Nothing, Date.now |> Task.perform RecieveDate )



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

        mymap dateRange myList acc =
            case ( dateRange, myList ) of
                ( d :: dli, m :: mli ) ->
                    let
                        dDate =
                            TDate.toTuple d.theDate

                        mDate =
                            TDate.toTuple m.theDate
                    in
                        if dDate == mDate then
                            mymap dli mli <| List.append acc [ m ]
                        else
                            mymap dli myList <| List.append acc [ d ]

                ( [], m :: mli ) ->
                    mymap [] mli <| List.append acc [ m ]

                ( d :: dli, [] ) ->
                    mymap dli [] <| List.append acc [ d ]

                ( [], [] ) ->
                    mymap dateRange myList acc

        combined =
            mymap datesList list []

        temp =
            Debug.log "list" combined
    in
        Model Dict.empty Dict.empty Nothing


initDayContentFromDate : Date -> DayContent
initDayContentFromDate d =
    DayContent 0 0 (text "") d


{-| Displays the Calendar

    Calendar.view

-}
view : Model -> Html CalendarMsg
view model =
    let
        dates =
            datesInRange (date 2018 1 1) (date 2018 1 31)

        -- indexed =
        --     getMonthGridFromDates dates
    in
        div [ calendarGrid ] <|
            -- List.append
            [ div
                [ gridAccessSpanCol 1 7 1 ]
                [ h1 [ calendarHeader ]
                    [ text "CALENDAR!!" ]
                ]
            ]



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
            ( { model | currentDate = Just (date (Date.year rDate) (getMonthInt (Date.month rDate)) (Date.day rDate)) }, Cmd.none )


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



{- STYLES
 -}


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



--TODO make this do dict int (internal month)
-- setters

groupDayContentByMonth : List DayContent -> List InternalMonth
groupDayContentByMonth content =
    let
        sortedByYear =
            List.Extra.groupWhile (\x y -> TDate.year x.theDate == TDate.year y.theDate) content

        sortedByMonth =
            List.map (\x -> List.Extra.groupWhile (\y z -> TDate.month y.theDate == TDate.month z.theDate) x) sortedByYear

        toInternalMonthList = List.map (\x -> List.map listToInternalMonth x)  sortedByMonth
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
                (InternalMonth (TDate.year item.theDate) (TDate.month item.theDate) contentDict)

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
