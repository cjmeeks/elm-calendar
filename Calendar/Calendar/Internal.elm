module Calendar.Internal exposing (..)

import Calendar.Types exposing (..)
import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Time.Date as TDate exposing (..)
import List.Extra
import Mouse exposing (Position)
import Calendar.Styles exposing (..)


initDayContentFromDate : Date -> DayContent a
initDayContentFromDate d =
    DayContent 0 0 (div [] []) d


viewMonth : CalendarModel a -> InternalMonth a -> List (Html (CalendarMsg a))
viewMonth model month =
    let
        moveStyle =
            case model.drag of
                Just { xIndex, startX, currentX, yIndex, startY, currentY } ->
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


viewDayContent : Int -> Int -> DayContent a -> CalendarModel a -> Html (CalendarMsg a)
viewDayContent idx idy dayContent model =
    let
        moveStyle =
            case model.drag of
                Just { xIndex, startX, currentX, yIndex, startY, currentY } ->
                    if xIndex == idx && yIndex == idy then
                        [ ( "transform", "translateX( " ++ toString (currentX - startX) ++ "px) translateY( " ++ toString (currentY - startY) ++ "px) translateZ(10px)" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        ]
                    else
                        []

                Nothing ->
                    []

        dayContentHtml =
            div
                [ style moveStyle
                , Attrs.map Drags <| Calendar.Styles.onMouseDown <| DragStart dayContent.dayIndex dayContent.weekIndex dayContent
                ]
                [ Html.map CustomMsg dayContent.content ]

        defaultHeader =
            h3 [ defaultHeaderStyle ] [ text <| dateToString dayContent.theDate ]

        innerHtml =
            if model.config.customDayHeader then
                [ dayContentHtml ]
            else
                [ defaultHeader
                , dayContentHtml
                ]
    in
        div [ gridAccess idy idx, gridItem ]
            innerHtml


updateDrags : DragMsg a -> CalendarModel a -> ( CalendarModel a, Cmd (CalendarMsg a) )
updateDrags msg model =
    case msg of
        DragStart idx idy moved pos ->
            ( { model
                | drag =
                    Just <|
                        { yIndex = idy
                        , startY = pos.y
                        , currentY = pos.y
                        , xIndex = idx
                        , startX = pos.x
                        , currentX = pos.x
                        }
                , movingContent = Just moved
              }
            , Cmd.none
            )

        DragAt pos ->
            { model
                | drag =
                    Maybe.map
                        (\{ xIndex, startX, yIndex, startY } ->
                            { yIndex = yIndex
                            , startY = startY
                            , currentY = pos.y
                            , xIndex = xIndex
                            , startX = startX
                            , currentX = pos.x
                            }
                        )
                        model.drag
            }
                ! []

        DragEnd pos ->
            case model.drag of
                Just { xIndex, startX, currentX, yIndex, startY, currentY } ->
                    let
                        headerSize =
                            (toFloat (model.size.height - model.config.customHeaderHeight)) * 0.05

                        ySize =
                            (toFloat model.size.height) - headerSize

                        calculateX =
                            (toFloat (model.size.width - model.config.customSidebarWidth)) / 7

                        calculateY =
                            (ySize / 6)

                        xOffset =
                            toFloat <| (round ((toFloat currentX) - (toFloat startX)))

                        yOffset =
                            toFloat <| (round ((toFloat currentY) - (toFloat startY)))

                        newModel =
                            moveItem
                                xIndex
                                (xOffset / calculateX)
                                yIndex
                                (yOffset / calculateY)
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


moveItem : Int -> Float -> Int -> Float -> CalendarModel a -> CalendarModel a
moveItem fromPosX offsetX fromPosY offsetY model =
    let
        indexedMonthContent =
            case Dict.get ( model.currentYear, model.currentMonth ) model.months of
                Just month ->
                    Dict.fromList <|
                        List.map (\x -> ( ( x.dayIndex, x.weekIndex ), x )) <|
                            Dict.values month.days

                Nothing ->
                    Dict.empty

        newX =
            (fromPosX + (round offsetX)) % 8

        newY =
            if ((fromPosY + (round offsetY)) % 8) > 5 then
                5
            else
                clamp 1 5 ((fromPosY + (round offsetY)) % 8)

        temp =
            Debug.log "(from, offset, newY)" ( fromPosY, offsetY, newY )

        ( newMonth, to, from ) =
            case model.movingContent of
                Just moved ->
                    case Dict.get ( newX, newY ) indexedMonthContent of
                        Just item ->
                            ( listToInternalMonth <|
                                Dict.values <|
                                    Dict.insert ( moved.dayIndex, moved.weekIndex ) { item | dayIndex = moved.dayIndex, weekIndex = moved.weekIndex, theDate = moved.theDate } <|
                                        Dict.insert ( newX, newY ) { moved | dayIndex = newX, weekIndex = newY, theDate = item.theDate } indexedMonthContent
                            , Just <| TDate.toTuple item.theDate
                            , Just <| TDate.toTuple moved.theDate
                            )

                        Nothing ->
                            if newY == 5 then
                                case Dict.get ( newX, (newY - 1) ) indexedMonthContent of
                                    Just item ->
                                        ( listToInternalMonth <|
                                            Dict.values <|
                                                Dict.insert ( moved.dayIndex, moved.weekIndex ) { item | dayIndex = moved.dayIndex, weekIndex = moved.weekIndex, theDate = moved.theDate } <|
                                                    Dict.insert ( newX, newY ) { moved | dayIndex = newX, weekIndex = newY - 1, theDate = item.theDate } indexedMonthContent
                                        , Just <| TDate.toTuple item.theDate
                                        , Just <| TDate.toTuple moved.theDate
                                        )

                                    Nothing ->
                                        ( listToInternalMonth <| Dict.values indexedMonthContent, Nothing, Nothing )
                            else
                                ( listToInternalMonth <| Dict.values indexedMonthContent, Nothing, Nothing )

                Nothing ->
                    ( listToInternalMonth <| Dict.values indexedMonthContent, Nothing, Nothing )

        newMonths =
            Dict.insert ( newMonth.year, newMonth.month ) newMonth model.months
    in
        { model | months = newMonths }


getFromAndToDates : Position -> CalendarModel a -> ( Maybe CalendarDate, Maybe CalendarDate )
getFromAndToDates pos model =
    case model.drag of
        Just { xIndex, startX, currentX, yIndex, startY, currentY } ->
            let
                calculateX =
                    (toFloat model.size.width) / 7

                calculateY =
                    (toFloat model.size.height) / 6

                ( fromPosX, offsetX, fromPosY, offsetY ) =
                    ( xIndex
                    , (((toFloat currentX) - (toFloat startX)) / calculateX)
                    , yIndex
                    , (((toFloat currentY) - (toFloat startY)) / calculateY)
                    )

                indexedMonthContent =
                    case Dict.get ( model.currentYear, model.currentMonth ) model.months of
                        Just month ->
                            Dict.fromList <|
                                List.map (\x -> ( ( x.dayIndex, x.weekIndex ), x )) <|
                                    Dict.values month.days

                        Nothing ->
                            Dict.empty

                newX =
                    (fromPosX + (round offsetX)) % 8

                newY =
                    (fromPosY + (round offsetY)) % 7

                ( to, from ) =
                    case model.movingContent of
                        Just moved ->
                            case Dict.get ( newX, newY ) indexedMonthContent of
                                Just item ->
                                    ( Just <| TDate.toTuple item.theDate
                                    , Just <| TDate.toTuple moved.theDate
                                    )

                                Nothing ->
                                    ( Nothing, Nothing )

                        Nothing ->
                            ( Nothing, Nothing )
            in
                case ( to, from ) of
                    ( Just ( ty, tm, td ), Just ( fy, fm, fd ) ) ->
                        ( Just <| CalendarDate ( ty, tm, td ), Just <| CalendarDate ( fy, fm, fd ) )

                    _ ->
                        ( Nothing, Nothing )

        Nothing ->
            ( Nothing, Nothing )


dateToString :
    Date
    -> String
dateToString date =
    String.join "-" [ toString <| TDate.month date, toString <| TDate.day date, toString <| TDate.year date ]


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


combineDateRangeWithListOfDayContent : List (DayContent a) -> List (DayContent a) -> List (DayContent a) -> List (DayContent a)
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


insertDumbyMonth : Int -> Int -> InternalMonth a
insertDumbyMonth year month =
    let
        dates =
            datesInRange (date year month 1) (date year month (daysInMonth year month))

        dayContent =
            Dict.fromList <|
                List.map (\x -> ( ( TDate.year x.theDate, TDate.month x.theDate, TDate.day x.theDate ), x )) <|
                    getMonthGridFromDates <|
                        List.map initDayContentFromDate dates
    in
        InternalMonth month year dayContent



--------------------------------------------------------------------------
{- date functions adapted from goilluminate/elm-fancy-daterangepicker
   https://github.com/GoIlluminate/elm-fancy-daterangepicker/blob/master/src/DateRangePicker/Date.elm
   these are adapted to use elm-community/elm-time funtions and date type
-}
--------------------------------------------------------------------------


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


groupDayContentByMonth : List (DayContent a) -> List (InternalMonth a)
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


listToInternalMonth : List (DayContent a) -> InternalMonth a
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


getMonthGridFromDates : List (DayContent a) -> List (DayContent a)
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
        getGridXY dates 1 []


updateContent : CalendarModel a -> CalendarModel a
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


updateInternalMonthGrid : InternalMonth a -> InternalMonth a
updateInternalMonthGrid month =
    let
        days =
            List.map Tuple.second <| Dict.toList month.days

        newDays =
            Dict.fromList <| List.map (\x -> ( TDate.toTuple x.theDate, x )) <| getMonthGridFromDates days
    in
        { month | days = newDays }


subHeaders : List (Html (CalendarMsg a))
subHeaders =
    let
        list =
            [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]
    in
        List.map (\x -> div [ class "calendar-weekday-header", subHeader, gridAccess 1 (dayToGridxPosition x) ] [ text <| dayToString x ]) list


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


dayToString : Weekday -> String
dayToString weekd =
    case weekd of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


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
