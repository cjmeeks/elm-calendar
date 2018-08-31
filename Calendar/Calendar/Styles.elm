module Calendar.Styles exposing (calendarDayGrid, calendarGrid, calendarHeader, defaultHeaderStyle, gridAccess, gridAccessSpanCol, gridAccessSpanRow, gridItem, headerButton, headerGrid, onMouseDown, subHeader, subHeaderGrid)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Mouse exposing (Position)



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
        []


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


calendarGrid : Int -> Attribute msg
calendarGrid height =
    style
        [ ( "display", "grid" )
        , ( "height", toString height ++ "px" )
        , ( "width", "100%" )
        , ( "grid-template-rows", "4% 4% 89%" )
        , ( "grid-template-columns", "13.2% 13.2% 13.2% 13.2% 13.2% 13.2%" )
        , ( "grid-gap", "1%" )
        , ( "user-select", "none" )
        , ( "box-sizing", "border-box" )
        ]


subHeaderGrid : Attribute msg
subHeaderGrid =
    style
        [ ( "display", "grid" )
        , ( "width", "100%" )
        , ( "grid-template-columns", "13.2% 13.2% 13.2% 13.2% 13.2% 13.2%" )
        , ( "grid-gap", "1%" )
        , ( "user-select", "none" )
        , ( "box-sizing", "border-box" )
        ]


calendarDayGrid : Int -> Attribute msg
calendarDayGrid height =
    style
        [ ( "display", "grid" )
        , ( "height", toString height ++ "px" )
        , ( "width", "100%" )
        , ( "grid-template-rows", "19% 19% 19% 19% 19%" )
        , ( "grid-template-columns", "13.2% 13.2% 13.2% 13.2% 13.2% 13.2%" )
        , ( "user-select", "none" )
        , ( "grid-gap", "1%" )
        , ( "box-sizing", "border-box" )
        ]


headerGrid : Attribute msg
headerGrid =
    style
        [ ( "display", "grid" )
        , ( "height", "100%" )
        , ( "width", "100%" )
        , ( "grid-template-columns", "10% 80% 10%" )
        , ( "box-sizing", "border-box" )
        ]


calendarHeader : Attribute msg
calendarHeader =
    style
        [ ( "margin", "0px" )
        , ( "text-align", "center" )
        , ( "height", "100%" )
        ]


subHeader : Attribute msg
subHeader =
    style
        [ ( "margin", "0" )
        , ( "text-align", "center" )
        , ( "height", "100%" )
        , ( "background-color", "lightgrey" )
        ]


headerButton : Attribute msg
headerButton =
    style
        [ ( "margin", "0px" )
        , ( "text-align", "center" )
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
