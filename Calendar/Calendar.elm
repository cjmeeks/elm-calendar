module Calendar exposing (view)

{-| This library is for a drag and drop calendar


# views

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| Displays the Calendar

    Calendar.view

-}
view : Html msg
view =
    div [ calendarGrid ]
        [ div [ gridAccess 1 1, gridItem ] [ text "1" ]
        , div [ gridAccess 1 2, gridItem ] [ text "2" ]
        , div [ gridAccess 1 3, gridItem ] [ text "3" ]
        , div [ gridAccess 1 4, gridItem ] [ text "4" ]
        , div [ gridAccess 1 5, gridItem ] [ text "5" ]
        , div [ gridAccess 1 6, gridItem ] [ text "6" ]
        , div [ gridAccess 1 7, gridItem ] [ text "7" ]
        , div [ gridAccess 2 1, gridItem ] [ text "1" ]
        , div [ gridAccess 2 2, gridItem ] [ text "2" ]
        , div [ gridAccess 2 3, gridItem ] [ text "3" ]
        , div [ gridAccess 2 4, gridItem ] [ text "4" ]
        , div [ gridAccess 2 5, gridItem ] [ text "5" ]
        , div [ gridAccess 2 6, gridItem ] [ text "6" ]
        , div [ gridAccess 2 7, gridItem ] [ text "7" ]
        , div [ gridAccess 3 1, gridItem ] [ text "1" ]
        , div [ gridAccess 3 2, gridItem ] [ text "2" ]
        , div [ gridAccess 3 3, gridItem ] [ text "3" ]
        , div [ gridAccess 3 4, gridItem ] [ text "4" ]
        , div [ gridAccess 3 5, gridItem ] [ text "5" ]
        , div [ gridAccess 3 6, gridItem ] [ text "6" ]
        , div [ gridAccess 3 7, gridItem ] [ text "7" ]
        , div [ gridAccess 4 1, gridItem ] [ text "1" ]
        , div [ gridAccess 4 2, gridItem ] [ text "2" ]
        , div [ gridAccess 4 3, gridItem ] [ text "3" ]
        , div [ gridAccess 4 4, gridItem ] [ text "4" ]
        , div [ gridAccess 4 5, gridItem ] [ text "5" ]
        , div [ gridAccess 4 6, gridItem ] [ text "6" ]
        , div [ gridAccess 4 7, gridItem ] [ text "7" ]
        , div [ gridAccess 5 1, gridItem ] [ text "1" ]
        , div [ gridAccess 5 2, gridItem ] [ text "2" ]
        , div [ gridAccess 5 3, gridItem ] [ text "3" ]
        , div [ gridAccess 5 4, gridItem ] [ text "4" ]
        , div [ gridAccess 5 5, gridItem ] [ text "5" ]
        , div [ gridAccess 5 6, gridItem ] [ text "6" ]
        , div [ gridAccess 5 7, gridItem ] [ text "7" ]
        ]



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
        , ( "grid-template-rows", "1fr 1fr 1fr 1fr 1fr" )
        , ( "grid-template-columns", "14.2% 14.2% 14.2% 14.2% 14.2% 14.2%" )
        ]
