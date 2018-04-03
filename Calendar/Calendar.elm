module Calendar 
    exposing 
        ( CalendarMsg(Drags)
        , DragMsg(DragEnd)
        , CalendarModel
        , CalendarDate(..)
        , MovedDates
        , initCalendarModel
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
        )

{-| This library is for a drag and drop calendar


# views

@docs view


# update

@docs update

 
# subscriptions

@docs subscriptions


# model

@docs CalendarModel, initCalendarModel, MovedDates


# functions

Inits and setters for customizing the html

@docs setDayContent, setCustomForwardButton, setCustomBackButton, turnOnCustomButtons, addDayContent, deleteDayContent
@docs catchToAndFromDates

# types

@docs CalendarMsg, CalendarDate, DragMsg

-}

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (..)
import Task exposing (..)
import Time.Date as TDate exposing (..)
import List.Extra
import Json.Decode as Decode
import Mouse exposing (Position)
import Window exposing (..)


{-|

    The Internal calendar model
-}
type alias CalendarModel =
    { months : Dict.Dict ( Int, Int ) InternalMonth
    , currentDate : Maybe TDate.Date
    , currentMonth : Int
    , currentYear : Int
    , drag : Maybe Drag
    , movingContent : Maybe DayContent
    , size : Window.Size
    , config : Config
    }


{-| Calendar msg
    MovedItem is exposed so that you can subscribe to it and update your side of things
    You will have to have a msg that is msg CalendarDate CalendarDate
    MovedItem FromDate ToDate
-}
type CalendarMsg
    = RecieveDate Date.Date
    | MonthForward
    | MonthBackward
    | Drags DragMsg
    | Resize Window.Size
    | DoNothing

{-|
    Wrapper for a tuple of (year,month,date)
-}
type CalendarDate = 
    CalendarDate (Int, Int, Int)

{-|
    Moved item holds the to and from CalendarDate
-}
type alias MovedDates =
    { to : CalendarDate
    , from : CalendarDate
    }
{-|
    Export DragEnd to determine when something has moved
-}
type DragMsg
    = DragStart Int Int DayContent Position
    | DragAt Position
    | DragEnd Position


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

--TODO 
-- make config have toggle drag and drop
-- make config have default window sizing or have custom div size -> stretch
-- have a way for user to see which content was moved to update their stuff

type alias Config =
    { customHeader : Bool
    , customButtons : Bool 
    , customStuff : CustomStuff
    , toggleDragging : Bool
    }

initConfig : Config
initConfig =
    { customHeader = False
    , customButtons = False
    , customStuff = initCustomStuff
    , toggleDragging = True
    }

{-|
    turnOnCustomButtons
-}
turnOnCustomButtons : CalendarModel -> CalendarModel
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
turnOffCustomButtons : CalendarModel -> CalendarModel
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
turnOnDragging : CalendarModel -> CalendarModel
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
turnOffDragging : CalendarModel -> CalendarModel
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
turnOffDefaultHeader : CalendarModel -> CalendarModel
turnOffDefaultHeader model =
    let
        config = 
            model.config

        newConfig = 
            { config | customHeader = True }
    in
        { model | config = newConfig }

type alias CustomStuff =
    { forwardButton : Html CalendarMsg
    , backButton : Html CalendarMsg
    , headerFormat : String
    }

initCustomStuff : CustomStuff
initCustomStuff = 
    { forwardButton = div [] []
    , backButton = div [] []
    , headerFormat = ""
    }

{-|
    setCustomForwardButton
-}
setCustomForwardButton : Html CalendarMsg -> CalendarModel -> CalendarModel
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
setCustomBackButton : Html CalendarMsg -> CalendarModel -> CalendarModel
setCustomBackButton btn model =
    let
        config = 
            model.config

        custom = 
            config.customStuff

        newButtons = 
            { custom | backButton = btn}

        newConfig = 
            { config | customButtons = True, customStuff = newButtons }
    in
        { model | config = newConfig }


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
    ( CalendarModel Dict.empty Nothing 0 0 Nothing Nothing (Window.Size 0 0) initConfig, Cmd.batch [ Date.now |> Task.perform RecieveDate, Task.perform (\x -> Resize x) Window.size ] )


{-|

    subscriptions : Calendar Subscriptions
-}
subscriptions : CalendarModel -> Sub CalendarMsg
subscriptions model =
    let
        dragSub =
            case model.drag of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.map Drags <| Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
    in
        Sub.batch [ Window.resizes Resize, dragSub ]



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
        CalendarModel Dict.empty Nothing 0 0 Nothing Nothing (Window.Size 0 0) initConfig


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

        getMonth =
            Dict.get ( model.currentYear, model.currentMonth ) model.months

        monthContent =
            case getMonth of
                Just item ->
                    viewMonth model item

                Nothing ->
                    [ text "There was an Error" ]

        listOfMonths =
            List.map Tuple.second 
                <| Dict.toList model.months

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
        
    in
        div [ calendarGrid, class "calendar-container" ] <|
            List.append
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
                        [ backButton ]
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
                        [ forwardButton ]
                    ]
                ]
                monthContent


viewMonth : CalendarModel -> InternalMonth -> List (Html CalendarMsg)
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


viewDayContent : Int -> Int -> DayContent -> CalendarModel -> Html CalendarMsg
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
                , Attrs.map Drags <| onMouseDown <| DragStart dayContent.dayIndex dayContent.weekIndex dayContent
                ]
                [ dayContent.content ]
        
        defaultHeader = 
            h3 [ defaultHeaderStyle ] [ text <| dateToString dayContent.theDate ] 

        innerHtml =
            if model.config.customHeader then
                [ dayContentHtml ]
            else
                [ defaultHeader
                , dayContentHtml
                ]
    in
        div [ gridAccess idy idx, gridItem ]
            innerHtml


{-| updates the Calendar

    Calendar.update

-}
update : CalendarMsg -> CalendarModel -> ( CalendarModel, Cmd CalendarMsg )
update msg model =
    case msg of
        RecieveDate rDate ->
            let
                (monthInt, yearInt) =
                    (getMonthInt (Date.month rDate), Date.year rDate)

                newMonths =
                    case Dict.get (yearInt, monthInt) model.months of
                        Just _ -> 
                            model.months
                            
                        Nothing ->
                            Dict.insert (yearInt, monthInt) (insertDumbyMonth yearInt monthInt) model.months
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
            { model | size = newSize }! []

        DoNothing ->
            model ! []


updateDrags : DragMsg -> CalendarModel -> ( CalendarModel, Cmd CalendarMsg )
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
                        calculateX = 
                            (toFloat model.size.width) / 7

                        calculateY =
                            (toFloat model.size.height) / 6


                        newModel =
                            moveItem
                                xIndex
                                (((toFloat currentX) - (toFloat startX)) / calculateX)
                                yIndex
                                (((toFloat currentY) - (toFloat startY)) / calculateY)
                                model
                    in
                        { newModel
                            | drag = Nothing
                            , movingContent = Nothing
                        }
                        ! [ ]

                Nothing ->
                    { model
                        | drag = Nothing
                    }
                        ! []


moveItem : Int -> Float -> Int -> Float -> CalendarModel -> CalendarModel
moveItem fromPosX offsetX fromPosY offsetY model =
    let
        indexedMonthContent =
            case Dict.get ( model.currentYear, model.currentMonth ) model.months of
                Just month ->
                    Dict.fromList 
                        <| List.map (\x -> ( ( x.dayIndex, x.weekIndex ), x ) ) 
                        <| Dict.values month.days

                Nothing ->
                    Dict.empty

        newX =
            (fromPosX + (round offsetX)) % 8

        newY =
            (fromPosY + (round offsetY)) % 7

        (newMonth, to, from) =
            case model.movingContent of
                Just moved ->
                    case Dict.get ( newX, newY ) indexedMonthContent of
                        Just item ->
                            (listToInternalMonth 
                                <| Dict.values 
                                <| Dict.insert ( moved.dayIndex, moved.weekIndex ) { item | dayIndex = moved.dayIndex, weekIndex = moved.weekIndex, theDate = moved.theDate } 
                                <| Dict.insert ( newX, newY ) { moved | dayIndex = newX, weekIndex = newY, theDate = item.theDate } indexedMonthContent
                            , Just <| TDate.toTuple item.theDate
                            , Just <| TDate.toTuple moved.theDate
                            )

                        Nothing ->
                            (listToInternalMonth <| Dict.values indexedMonthContent,Nothing, Nothing)

                Nothing ->
                    (listToInternalMonth <| Dict.values indexedMonthContent, Nothing, Nothing)

        newMonths =
            Dict.insert ( newMonth.year, newMonth.month ) newMonth model.months
    
    in
        { model | months = newMonths } 

{-|
   Catch the from date and to date of the moved content for use by the user
-}
catchToAndFromDates : CalendarMsg -> CalendarModel -> Maybe MovedDates
catchToAndFromDates msg model =
    case msg of
        Drags dMsg ->
            case dMsg of
                DragEnd pos ->
                    let
                        (toDate, fromDate) =
                            getFromAndToDates pos model
                    in
                        case (toDate, fromDate) of
                            (Just toCalendarDate, Just fromCalendarDate) ->
                                Just <| MovedDates toCalendarDate fromCalendarDate

                            _ -> 
                                Nothing

                _ -> 
                    Nothing

        _ -> 
            Nothing
        


getFromAndToDates : Position -> CalendarModel -> (Maybe CalendarDate, Maybe CalendarDate)
getFromAndToDates pos model =
    case model.drag of
        Just { xIndex, startX, currentX, yIndex, startY, currentY } ->
            let
                calculateX = 
                    (toFloat model.size.width) / 7

                calculateY =
                    (toFloat model.size.height) / 6
                
                (fromPosX, offsetX, fromPosY, offsetY) =
                    ( xIndex
                    , (((toFloat currentX) - (toFloat startX)) / calculateX)
                    , yIndex
                    , (((toFloat currentY) - (toFloat startY)) / calculateY)
                    )

                indexedMonthContent =
                    case Dict.get ( model.currentYear, model.currentMonth ) model.months of
                        Just month ->
                            Dict.fromList 
                                <| List.map (\x -> ( ( x.dayIndex, x.weekIndex ), x ) ) 
                                <| Dict.values month.days

                        Nothing ->
                            Dict.empty

                newX =
                    (fromPosX + (round offsetX)) % 8

                newY =
                    (fromPosY + (round offsetY)) % 7

                (to, from) =
                    case model.movingContent of
                        Just moved ->
                            case Dict.get ( newX, newY ) indexedMonthContent of
                                Just item ->
                                    ( Just <| TDate.toTuple item.theDate
                                    , Just <| TDate.toTuple moved.theDate
                                    )

                                Nothing ->
                                    (Nothing, Nothing)

                        Nothing ->
                            (Nothing, Nothing)
            in
                 case (to,from) of
                    (Just (ty,tm,td), Just (fy,fm,fd)) ->
                        (Just <| CalendarDate (fy,fm,fd), Just <| CalendarDate (ty,tm,td))

                    _ -> 
                        (Nothing, Nothing)
                
        Nothing ->
            (Nothing, Nothing)

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
        , ( "user-select", "none" )
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
        , ( "height", "100%" )
        , ( "border-bottom-style", "solid" )
        , ( "border-top-style", "solid" )
        , ( "border-width", "1px" )
        ]

headerButton : Attribute msg
headerButton =
    style
        [ ( "margin", "0px" )
        , ( "text-align", "center" )
        , ( "border-style", "solid" )
        , ( "border-width", "1px" )
        , ( "height", "100%" )
        , ( "font-size", "40px" )
        , ( "background-color", "lightgrey" )
        ]

defaultHeaderStyle : Attribute msg
defaultHeaderStyle =
    style
        [ ( "background-color", "lightgrey" )
        , ( "margin", "0px" )
        , ( "text-align", "center" )
        ]

onMouseDown : (Position -> msg) -> Attribute msg
onMouseDown msg =
    on "mousedown" (Decode.map msg Mouse.position)


-- Date Stuff

listOfMonthInts : List Int
listOfMonthInts =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]

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

{-| Takes a list of day content and sets that as the calendar content
-}
setDayContent : List (CalendarDate, Html CalendarMsg) -> CalendarModel -> CalendarModel
setDayContent days model =
    let
        ( min, max ) =
            getMinAndMaxDate <| List.map (\(CalendarDate (year, month, d), h) -> TDate.date year month d) days

        dates =
            datesInRange min max

        datesList =
            List.map initDayContentFromDate dates

        daysList = List.sortWith (\t1 t2 -> TDate.compare t1.theDate t2.theDate) <| List.map (\(CalendarDate (year,month,d), c) -> DayContent 0 0 c (TDate.date year month d)) days 

        combined =
            combineDateRangeWithListOfDayContent datesList daysList []

        daysToInternalMonths =
            groupDayContentByMonth combined

        gridStuff =
            List.map (\x -> { x | days = Dict.fromList 
                                            <| List.map (\y -> ( ( TDate.year y.theDate, TDate.month y.theDate, TDate.day y.theDate ), y )) 
                                            <| getMonthGridFromDates 
                                            <| List.map Tuple.second 
                                            <| Dict.toList x.days
                             }) daysToInternalMonths

        monthDict =
            Dict.fromList <| List.map (\x -> ( ( x.year, x.month ), x )) gridStuff
    in
        { model | months = monthDict }




insertDumbyMonth : Int -> Int -> InternalMonth
insertDumbyMonth year month =
    let
        dates =
            datesInRange (date year month 1) (date year month (daysInMonth year month))

        dayContent =
            Dict.fromList 
                <| List.map (\x -> ( ( TDate.year x.theDate, TDate.month x.theDate, TDate.day x.theDate ), x ) ) 
                <| getMonthGridFromDates 
                <| List.map initDayContentFromDate dates
    in
        InternalMonth month year dayContent

{-| Takes a tuple of (year, month, date) and Html content corresponding to that date
-}
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


{-| Takes a tuple of (year, month, date) to be deleted
-}
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
