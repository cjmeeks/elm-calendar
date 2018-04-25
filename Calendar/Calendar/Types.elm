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
type alias CalendarModel a =
    { months : Dict.Dict ( Int, Int ) (InternalMonth a)
    , currentDate : Maybe TDate.Date
    , currentMonth : Int
    , currentYear : Int
    , drag : Maybe Drag
    , movingContent : Maybe (DayContent a)
    , size : Window.Size
    , config : Config a
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
type alias Config a =
    { customDayHeader : Bool
    , customButtons : Bool
    , customStuff : CustomStuff a
    , toggleDragging : Bool
    , customHeaderHeight : Int
    , customSidebarWidth : Int
    }


{-| Custom type for user custom html
-}
type alias CustomStuff a =
    { forwardButton : Html a
    , backButton : Html a
    , headerFormat : String
    }


{-| Calendar msg
MovedItem is exposed so that you can subscribe to it and update your side of things
You will have to have a msg that is msg CalendarDate CalendarDate
MovedItem FromDate ToDate
-}
type CalendarMsg a
    = RecieveDate Date.Date
    | MonthForward
    | MonthBackward
    | Drags (DragMsg a)
    | Resize Window.Size
    | CustomMsg a
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
type DragMsg a
    = DragStart Int Int (DayContent a) Position
    | DragAt Position
    | DragEnd Position


{-| Data related to each Day
-}
type alias DayContent a =
    { dayIndex : Int
    , weekIndex : Int
    , content : Html a
    , theDate : TDate.Date
    }


{-|

    key of days is (year,month,day)
-}
type alias InternalMonth a =
    { month : Int
    , year : Int
    , days : Dict.Dict ( Int, Int, Int ) (DayContent a)
    }
