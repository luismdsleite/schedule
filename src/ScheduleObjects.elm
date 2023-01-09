module ScheduleObjects exposing (..)

import Table exposing (..)
import Time exposing (..)


{-| A schedule here is designated as a block.
A block is composed of events. An event is comprised of a subject, given in a specific room by a lecturer during a given time.
-}
type alias Block =
    { name : String, nameAbbr : String, cond : Event -> Bool }


type alias Data =
    { rooms : Table Room
    , lecturers : Table Lecturer
    , events : Table Event
    , blocks : Table Block
    }


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
    { name : String, abbr : String, goodTime : List WeekTime, difficultTime : List WeekTime, unavailableTime : List WeekTime }


{-| A room has an ID, a name, a abbreviation and a capacity.
-}
type alias Room =
    { name : String, abbr : String, capacity : Int }


type alias LecturerID =
    ID


type alias EventID =
    ID


type alias RoomID =
    ID



---- Filters ----


{-| A Filter is a specific view of all events (e.g. show all events from room X or show all events whose lecturer is C).
The ScheduleFilter holds 3 different filter functions that are only parsed when displaying html.
-}
type ScheduleFilter
    = ScheduleFilter RoomFilter LecturerFilter BlockFilter RoomName LecturerName BlockName


type alias RoomFilter =
    Int -> Event -> Bool


type alias LecturerFilter =
    Int -> Event -> Bool


type alias BlockFilter =
    Int -> Event -> Bool


type alias RoomName =
    String


type alias LecturerName =
    String


type alias BlockName =
    String



---- weekDay converting functions ----


{-| The days of the week are represented as Time.WeekTime and stored in a `WeekTime.weekday`. This function converts them into a Portuguese `String`.

    convertWeekDay Time.Mon == "Seg"

    convertWeekDay Time.Tue == "Ter"

-}
convertWeekDay : Maybe WeekTime -> String
convertWeekDay weekTime =
    case weekTime of
        Nothing ->
            "-----"

        Just val ->
            toPortugueseWeekday val.weekday


toPortugueseWeekday : Time.Weekday -> String
toPortugueseWeekday weekday =
    case weekday of
        Mon ->
            "Seg"

        Tue ->
            "Ter"

        Wed ->
            "Qua"

        Thu ->
            "Qui"

        Fri ->
            "Sex"

        Sat ->
            "Sáb"

        Sun ->
            "Dom"


toCssClassWeekDay : Time.Weekday -> String
toCssClassWeekDay weekday =
    case weekday of
        Mon ->
            "mon"

        Tue ->
            "tue"

        Wed ->
            "wed"

        Thu ->
            "thu"

        Fri ->
            "fri"

        Sat ->
            "sat"

        Sun ->
            "sun"


{-| The Hours and Minutes are represented as an `Int` as part of the `WeekTime` record. This function converts them into a `String`

    convertHourAndMinute 9 0 == "09:00"

    convertHourAndMinute 10 30 == "10:30"

-}
convertWeekTimeHourAndMinute : Maybe WeekTime -> String
convertWeekTimeHourAndMinute time =
    case time of
        Nothing ->
            "----"

        Just val ->
            convertHourAndMinute val.hour val.minute


convertHourAndMinute : Int -> Int -> String
convertHourAndMinute hour minute =
    let
        hourStr =
            if hour < 10 then
                "0" ++ String.fromInt hour

            else
                String.fromInt hour

        minuteStr =
            if minute < 10 then
                String.fromInt minute ++ "0"

            else
                String.fromInt minute
    in
    hourStr ++ ":" ++ minuteStr


sortByWeekday : List ( Int, Event ) -> Time.Weekday -> List ( Int, Event )
sortByWeekday allEvents weekDay =
    let
        filter : Time.Weekday -> ( Int, Event ) -> Bool
        filter weekday ( id, ev ) =
            case ev.start_time of
                Just time ->
                    time.weekday == weekday

                Nothing ->
                    False
    in
    List.filter (filter weekDay) allEvents


displayedWeekDays : List Weekday
displayedWeekDays =
    [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri ]
