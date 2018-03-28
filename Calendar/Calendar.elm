module Calendar exposing (CalendarMsg, DayContent, CalendarModel, initCalendarModel, update, view, dateToString, setDayContent, subscriptions)

{-| This library is for a drag and drop calendar


# views 

@docs view


# update

@docs update

# subscriptions

@docs subscriptions
# model

@docs CalendarModel, DayContent


# functions

@docs initCalendarModel, dateToString, setDayContent


# types

@docs CalendarMsg

-}

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (..)
import Task exposing (..)
import Time.Date as TDate exposing (..)
import Set
import List.Extra
import Json.Decode as Decode
import Mouse exposing (Position)


{-|

    The model
    content is a dictionary of key: (Year, Month) to Value : Internal Month
-}
type alias CalendarModel =
    { months : Dict.Dict ( Int, Int ) InternalMonth
    , currentDate : Maybe TDate.Date
    , currentMonth : Int
    , currentYear : Int
    , drag : Maybe Drag
    , movingContent : Maybe DayContent
    }


{-| Calendar msg
-}
type CalendarMsg
    = RecieveDate Date.Date
    | MonthForward
    | MonthBackward
    | Drags DragMsg
    | DoNothing


type DragMsg
    = DragStart Int Int DayContent Position
    | DragAt Position
    | DragEnd Position


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


{-|

    key of days is (year,month,day)
-}
type alias InternalMonth =
    { month : Int
    , year : Int
    , days : Dict.Dict ( Int, Int, Int ) DayContent
    }



--item index is ()


type alias Drag =
    { xIndex : Int
    , startX : Int
    , currentX : Int
    , yIndex : Int
    , startY : Int
    , currentY : Int
    }


{-|

    The daycontent
    pass in msg type of your html
-}
initCalendarModel : ( CalendarModel, Cmd CalendarMsg )
initCalendarModel =
    ( CalendarModel Dict.empty Nothing 0 0 Nothing Nothing, Date.now |> Task.perform RecieveDate )


{-|
    subscriptions : Calendar Subscriptions
-}
subscriptions : CalendarModel -> Sub CalendarMsg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.map Drags <| Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

{-
   initial function to get the daterange
-}


initWithDayContent : List DayContent -> CalendarModel
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
        CalendarModel Dict.empty Nothing 0 0 Nothing Nothing


initDayContentFromDate : Date -> DayContent
initDayContentFromDate d =
    DayContent 0 0 (div [] []) d


{-| Displays the Calendar

    Calendar.view

-}
view : CalendarModel -> Html CalendarMsg
view model =
    let
        dates =
            datesInRange (date 2018 1 1) (date 2018 1 31)

        listOfMonths =
            List.map Tuple.second <| Dict.toList model.months

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
            Dict.get ( model.currentYear, model.currentMonth ) model.months

        monthContent =
            case getMonth of
                Just item ->
                    viewMonth model item

                Nothing ->
                    [ text "There was an Error" ]
    in
        div [ calendarGrid, class "calendar-container" ] <|
            List.append
                [ div
                    [ gridAccessSpanCol 1 7 1
                    , headerGrid
                    , class "calendar-header-container"
                    ]
                    [ button
                        [ onClick MonthBackward
                        , gridAccess 1 1
                        , class "calendar-header-back-button"
                        ]
                        [ text "<<" ]
                    , h1
                        [ calendarHeader
                        , gridAccess 1 2
                        , class "calendar-header-title"
                        ]
                        [ text <| monthToString model.currentMonth ]
                    , button
                        [ onClick MonthForward
                        , gridAccess 1 3
                        , class "calendar-header-forward-button"
                        ]
                        [ text ">>" ]
                    ]
                ]
                monthContent


viewMonth : CalendarModel -> InternalMonth -> List (Html CalendarMsg)
viewMonth model month =
    let
        moveStyle =
            case model.drag of
                Just { xIndex, startX, currentX , yIndex, startY , currentY } ->
                    -- if itemIndex == idx then
                        [ ( "transform", "translateX( " ++ toString (currentX - startX) ++ "px) translateY( " ++ toString (currentY - startY) ++ "px) translateZ(10px)" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        ]
                Nothing ->
                    []

    in 
        List.map
            (\( x, dayContent ) ->
                viewDayContent dayContent.dayIndex dayContent.weekIndex dayContent model
            )
        <|
            Dict.toList month.days

viewDayContent : Int -> Int -> DayContent -> CalendarModel -> Html CalendarMsg
viewDayContent idx idy dayContent model =
    let
        moveStyle =
            case model.drag of
                Just { xIndex, startX, currentX , yIndex, startY , currentY } ->
                    if xIndex == idx && yIndex == idy then
                        [ ( "transform", "translateX( " ++ toString (currentX - startX) ++ "px) translateY( " ++ toString (currentY - startY) ++ "px) translateZ(10px)" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        ]
                    else 
                        []
                Nothing ->
                    []
    in
        div [ gridAccess idy idx, gridItem ]
            [ h1 [] [ text <| dateToString dayContent.theDate ] --h1 tag to be deleted
            , div
                [ style moveStyle
                , Attrs.map Drags <| onMouseDown <| DragStart dayContent.dayIndex dayContent.weekIndex dayContent
                ] 
                [dayContent.content]
            ]

        
onMouseDown : (Position -> msg) -> Attribute msg
onMouseDown msg =
    on "mousedown" (Decode.map msg Mouse.position)


{-| updates the Calendar

    Calendar.update

-}
update : CalendarMsg -> CalendarModel -> ( CalendarModel, Cmd CalendarMsg )
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
                    case Dict.get ( newYear, newMonth ) model.months of
                        Just m ->
                            model.months

                        Nothing ->
                            Dict.insert ( newYear, newMonth ) (insertDumbyMonth newYear newMonth) model.months
            in
                ( { model | currentMonth = newMonth, currentYear = newYear, months = updatedContent }, Cmd.none )

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
                ( { model | currentMonth = newMonth, currentYear = newYear, months = updatedContent }, Cmd.none )

        Drags msg_ ->
            updateDrags msg_ model

        DoNothing ->
            ( model, Cmd.none )


updateDrags : DragMsg -> CalendarModel -> (CalendarModel, Cmd CalendarMsg)
updateDrags msg model =
    case msg of
        DragStart idx idy moved pos ->
            ({model | drag = Just <| 
                                { yIndex = idy
                                , startY = pos.y
                                , currentY = pos.y
                                , xIndex = idx
                                , startX = pos.x
                                , currentX = pos.x 
                                }
                    , movingContent = Just moved
            }, Cmd.none)
    
        DragAt pos ->
            { model
                | drag =
                    Maybe.map (\{ xIndex, startX, yIndex, startY  } -> 
                                { yIndex = yIndex
                                , startY = startY 
                                , currentY = pos.y
                                , xIndex = xIndex
                                , startX = startX
                                , currentX = pos.x 
                                }) model.drag
            }
                ! []
        DragEnd pos ->
            case model.drag of
                Just { xIndex, startX, currentX, yIndex, startY, currentY } -> --x =270 y=170
                    let
                        temp = Debug.log "drag" model.drag
                        newModel =
                             moveItem
                                xIndex
                                (((toFloat currentX) - (toFloat startX)) / 260)
                                yIndex
                                (((toFloat currentY) - (toFloat startY)) / 180)
                                model
                    in
                    { newModel
                        | drag = Nothing
                        , movingContent = Nothing
                    }
                        ! []

                Nothing ->
                    { model
                        | drag = Nothing
                    }
                        ! []

moveItem : Int -> Float -> Int -> Float -> CalendarModel -> CalendarModel
moveItem fromPosX offsetX fromPosY offsetY model =
    let
        indexedMonthContent = case Dict.get (model.currentYear, model.currentMonth) model.months of
            Just month ->
                Dict.fromList <| List.map (\x -> ((x.dayIndex, x.weekIndex), x)) <| Dict.values month.days        
            Nothing ->
                Dict.empty
            
        temp = (Debug.log "x" (fromPosX, offsetX), Debug.log "y" (fromPosY, offsetY)  )
        
        newX = (fromPosX + (round offsetX)) % 8

        -- temp = (Debug.log "fromposx" fromPosX, Debug.log "offsetX" offsetX,Debug.log "newX" newX)
        newY = (fromPosY + (round offsetY)) % 7
        
        _ = (Debug.log "new" (newX, newY))
        --TODO do something else when content is already there
        newMonth = 
            case model.movingContent of
                Just moved -> 
                    case Dict.get (newX, newY) indexedMonthContent of
                        Just item ->
                            listToInternalMonth <| Dict.values <| Dict.insert (moved.dayIndex, moved.weekIndex) {item | dayIndex = moved.dayIndex, weekIndex = moved.weekIndex, theDate = moved.theDate} <| Dict.insert (newX, newY) { moved | dayIndex = newX, weekIndex = newY, theDate = item.theDate } indexedMonthContent 

                        Nothing -> listToInternalMonth <| Dict.values indexedMonthContent 
                            
                        
                Nothing -> listToInternalMonth <| Dict.values indexedMonthContent 

        newMonths = Dict.insert (newMonth.year, newMonth.month) newMonth model.months

    in
        { model | months = newMonths }




      
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
        , ( "user-select", "none")
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
setDayContent : List DayContent -> CalendarModel -> CalendarModel
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
        { model | months = monthDict }



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


addDayContent : ( Int, Int, Int ) -> Html CalendarMsg -> CalendarModel -> CalendarModel
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



--DeleteDayContent


deleteDayContent : ( Int, Int, Int ) -> CalendarModel -> CalendarModel
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


updateContent : CalendarModel -> CalendarModel
updateContent model =
    let
        listOfMonths =
            Dict.toList model.months

        newListOfMonths =
            List.map
                (\( key, x ) ->
                    ( key, updateInternalMonthGrid x )
                )
                listOfMonths
    in
        { model | months = Dict.fromList newListOfMonths }


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


monthToString : Int -> String
monthToString d =
    case d of
        1 ->
            "January"

        2 ->
            "Febuary"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            "Internal Error"


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
