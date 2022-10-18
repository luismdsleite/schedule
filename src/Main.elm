module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)



---- MODEL ----
-- TODO: Replace Lists with dictionaries (to search by id)
-- TODO: Add missing data in renderEvent
-- TODO: Remove data in init and read from a json file
-- TODO: Fix names, comment css code


type alias Model =
    { rooms : Dict Int SC_room
    , lecturers : List SC_lecturer
    , events : List SC_event
    , blocks : List SC_block
    }


{-| A schedule here is designated as a block.
A block is composed of events. An event is comprised of a subject, given in a specific room by a lecturer during a given time.
-}
type alias SC_block =
    { name : String, abbr : String, events : List SC_event }


type alias SC_event =
    { subject : String
    , subjectAbbr : String
    , room : Maybe Int
    , start_time : Maybe SC_time
    , end_time : Maybe SC_time
    , lecturer : Maybe Int
    }


type alias SC_time =
    { weekday : Int, hour : Int, minute : Int }


{-| The days of the week are represented as an `Int` and stored in a SC\_time.weekday. This function converts them into a `String`.

    convertWeekDay 1 == "Segunda"

    convertWeekDay 5 == "Sexta"

-}
convertWeekDay : Int -> String
convertWeekDay weekday =
    case weekday of
        1 ->
            "Segunda"

        2 ->
            "Terça"

        3 ->
            "Quarta"

        4 ->
            "Quinta"

        5 ->
            "Sexta"

        6 ->
            "Sabado"

        7 ->
            "Domingo"

        _ ->
            "Invalid weekday!"


{-| The Hours and Minutes are represented as an `Int` as part of the SC\_time record. This function converts them into a `String`

    convertHourAndMinute 9 0 == "09:00"

    convertHourAndMinute 10 30 == "10:30"

-}
convertHourAndMinute : Maybe SC_time -> String
convertHourAndMinute time =
    case time of
        Nothing ->
            "----"

        Just val ->
            let
                hourStr =
                    if val.hour < 10 then
                        "0" ++ String.fromInt val.hour

                    else
                        String.fromInt val.hour

                minuteStr =
                    if val.minute < 10 then
                        String.fromInt val.minute ++ "0"

                    else
                        String.fromInt val.minute
            in
            hourStr ++ ":" ++ minuteStr


{-| A room has an ID, a name and a abbreviation.
-}
type alias SC_room =
    { id : Int, name : String, abbr : String, capacity : Int }


{-| A lecturer/teacher has an ID, a name and a abbreviation.
-}
type alias SC_lecturer =
    { id : Int, name : String, abbr : String }




init : ( Model, Cmd Msg )
init =
    ( { rooms = Dict.fromList [ ( 1, SC_room 1 "DCC Lab. 2" "FC6_157 (Lab2)" 20 ) ]
      , lecturers = [ SC_lecturer 1 "N'Golo Kanté" "NGK" ]
      , events =
            [ SC_event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just 1) (Just (SC_time 1 9 30)) (Just (SC_time 1 11 0)) (Just 1) ]
      , blocks = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is workng!" ]
        , div [ class "listbox-area" ]
            [ renderList [ "hello", "how", "are", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you", "you" ]
            , renderEvents model.events model.rooms model.lecturers
            ]
        ]


renderList : List String -> Html msg
renderList lst =
    ul [ class "list custom-scrollbar" ] (List.map (\l -> li [ class "list-item" ] [ text l ]) lst)


renderEvents : List SC_event -> Dict Int SC_room -> List SC_lecturer -> Html msg
renderEvents events rooms lecturers =
    ul [ class "list custom-scrollbar" ]
        (List.map renderEvent events)


{-| Transforms an event into a list item
-}
renderEvent : SC_event -> Html msg
renderEvent event =
    li [ class "list-item" ]
        [ div [] [ text event.subjectAbbr ]
        , div [] [ text event.subject ]
        , div [] [ text (convertHourAndMinute event.start_time) ]
        , div [] [ text (convertHourAndMinute event.end_time) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
