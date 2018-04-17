module Calendar.Types
    exposing
        ( CalendarModel
        , MovedDates
        , Drag
        , Config
        , CustomStuff
        , DragMsg(..)
        , CalendarMsg(..)
        , CalendarDate(..)
        , DayContent
        , InternalMonth
        )

{-| This library is for a drag and drop calendar


# model

@docs CalendarModel, MovedDates, Drag, Config, CustomStuff, DragMsg, DayContent, InternalMonth


# types

@docs CalendarMsg, CalendarDate, DragMsg

-}

import Time.Date as TDate exposing (..)
import Html exposing (..)
import Dict
import Window
import Date
import Mouse exposing (Position)


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


{-| Drag type alias for the drags
-}
type alias Drag =
    { xIndex : Int
    , startX : Int
    , currentX : Int
    , yIndex : Int
    , startY : Int
    , currentY : Int
    }


{-| Config for library
-}
type alias Config =
    { customHeader : Bool
    , customButtons : Bool
    , customStuff : CustomStuff
    , toggleDragging : Bool
    }


{-| Custom type for user custom html
-}
type alias CustomStuff =
    { forwardButton : Html CalendarMsg
    , backButton : Html CalendarMsg
    , headerFormat : String
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
type CalendarDate
    = CalendarDate ( Int, Int, Int )


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


{-| Data related to each Day
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
