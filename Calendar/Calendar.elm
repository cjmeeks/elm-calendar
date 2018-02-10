module Calendar exposing (DayContent, Model, initCalendarModel, view)

{-| This library is for a drag and drop calendar


# views

@docs view


# model

@docs Model
@docs DayContent


# functions

@docs initCalendarModel

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)


{-|

    The model
    pass in msg type of your html

-}
type alias Model a =
    { content : Dict.Dict Int (Dict.Dict Int (DayContent a))
    }


type Msg
    = CustomMsg


{-|

    The daycontent
    pass in msg type of your html

-}
type alias DayContent a =
    { index : Int
    , content : Html a
    }


{-|

    The daycontent
    pass in msg type of your html

-}
initCalendarModel : Html a -> Model a
initCalendarModel defaultHtml =
    let
        one =
            1

        two =
            2

        three =
            3

        four =
            4

        five =
            5

        six =
            6

        seven =
            7

        dict =
            Dict.fromList
                [ ( one
                  , Dict.fromList
                        [ ( one, DayContent one (div [] [ defaultHtml ]) )
                        , ( two, DayContent two (div [] [ defaultHtml ]) )
                        , ( three, DayContent three (div [] [ defaultHtml ]) )
                        , ( four, DayContent four (div [] [ defaultHtml ]) )
                        , ( five, DayContent five (div [] [ defaultHtml ]) )
                        , ( six, DayContent six (div [] [ defaultHtml ]) )
                        , ( seven, DayContent seven (div [] [ defaultHtml ]) )
                        ]
                  )
                , ( 2
                  , Dict.fromList
                        [ ( one, DayContent one (div [] [ defaultHtml ]) )
                        , ( two, DayContent two (div [] [ defaultHtml ]) )
                        , ( three, DayContent three (div [] [ defaultHtml ]) )
                        , ( four, DayContent four (div [] [ defaultHtml ]) )
                        , ( five, DayContent five (div [] [ defaultHtml ]) )
                        , ( six, DayContent six (div [] [ defaultHtml ]) )
                        , ( seven, DayContent seven (div [] [ defaultHtml ]) )
                        ]
                  )
                , ( 3
                  , Dict.fromList
                        [ ( one, DayContent one (div [] [ defaultHtml ]) )
                        , ( two, DayContent two (div [] [ defaultHtml ]) )
                        , ( three, DayContent three (div [] [ defaultHtml ]) )
                        , ( four, DayContent four (div [] [ defaultHtml ]) )
                        , ( five, DayContent five (div [] [ defaultHtml ]) )
                        , ( six, DayContent six (div [] [ defaultHtml ]) )
                        , ( seven, DayContent seven (div [] [ defaultHtml ]) )
                        ]
                  )
                , ( 4
                  , Dict.fromList
                        [ ( one, DayContent one (div [] [ defaultHtml ]) )
                        , ( two, DayContent two (div [] [ defaultHtml ]) )
                        , ( three, DayContent three (div [] [ defaultHtml ]) )
                        , ( four, DayContent four (div [] [ defaultHtml ]) )
                        , ( five, DayContent five (div [] [ defaultHtml ]) )
                        , ( six, DayContent six (div [] [ defaultHtml ]) )
                        , ( seven, DayContent seven (div [] [ defaultHtml ]) )
                        ]
                  )
                , ( 5
                  , Dict.fromList
                        [ ( one, DayContent one (div [] [ defaultHtml ]) )
                        , ( two, DayContent two (div [] [ defaultHtml ]) )
                        , ( three, DayContent three (div [] [ defaultHtml ]) )
                        , ( four, DayContent four (div [] [ defaultHtml ]) )
                        , ( five, DayContent five (div [] [ defaultHtml ]) )
                        , ( six, DayContent six (div [] [ defaultHtml ]) )
                        , ( seven, DayContent seven (div [] [ defaultHtml ]) )
                        ]
                  )
                ]
    in
    Model dict


{-| Displays the Calendar

    Calendar.view

-}
view : Model msg -> Html msg
view model =
    let
        row1 =
            getRow 1 model.content

        row2 =
            getRow 2 model.content

        row3 =
            getRow 3 model.content

        row4 =
            getRow 4 model.content

        row5 =
            getRow 5 model.content
    in
    div [ calendarGrid ] <|
        List.concat
            [ viewRow 1 row1
            , viewRow 2 row2
            , viewRow 3 row3
            , viewRow 4 row4
            , viewRow 5 row5
            ]


getRow : Int -> Dict.Dict Int (Dict.Dict Int (DayContent a)) -> List ( Int, DayContent a )
getRow row dict =
    case Dict.get row dict of
        Just theRow ->
            Dict.toList theRow

        Nothing ->
            []


viewRow : Int -> List ( Int, DayContent a ) -> List (Html a)
viewRow rowIndex row =
    List.map (\( col, day ) -> div [ gridAccess rowIndex day.index, gridItem ] [ day.content ]) row



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
