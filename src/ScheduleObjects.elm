module ScheduleObjects exposing (..)

import Table exposing (..)
import Time


{-| A schedule here is designated as a block.
A block is composed of events. An event is comprised of a subject, given in a specific room by a lecturer during a given time.
-}
type alias Block =
    { name : String, abbr : String, events : List Event, cond : Bool }


type alias Event =
    { subject : String
    , subjectAbbr : String
    , room : Maybe RoomID
    , start_time : Maybe WeekTime
    , end_time : Maybe WeekTime
    , lecturer : Maybe LecturerID
    }


type alias WeekTime =
    { weekday : Time.Weekday, hour : Int, minute : Int }


{-| A lecturer/teacher has an ID, a name and a abbreviation.
-}
type alias Lecturer =
    { name : String, abbr : String }


type alias LecturerID =
    ID


{-| A room has an ID, a name, a abbreviation and a capacity.
-}
type alias Room =
    { name : String, abbr : String, capacity : Int }


type alias RoomID =
    ID



---- weekDay converting functions ----


{-| The days of the week are represented as an `Int` and stored in a `WeekTime.weekday`. This function converts them into a `String`.

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


{-| The Hours and Minutes are represented as an `Int` as part of the `WeekTime` record. This function converts them into a `String`

    convertHourAndMinute 9 0 == "09:00"

    convertHourAndMinute 10 30 == "10:30"

-}
convertHourAndMinute : Maybe WeekTime -> String
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
