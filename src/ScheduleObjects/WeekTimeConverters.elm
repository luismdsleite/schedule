module ScheduleObjects.WeekTimeConverters exposing (..)

import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time



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
        Time.Mon ->
            "Seg"

        Time.Tue ->
            "Ter"

        Time.Wed ->
            "Qua"

        Time.Thu ->
            "Qui"

        Time.Fri ->
            "Sex"

        Time.Sat ->
            "Sáb"

        Time.Sun ->
            "Dom"


toCssClassWeekDay : Time.Weekday -> String
toCssClassWeekDay weekday =
    case weekday of
        Time.Mon ->
            "mon"

        Time.Tue ->
            "tue"

        Time.Wed ->
            "wed"

        Time.Thu ->
            "thu"

        Time.Fri ->
            "fri"

        Time.Sat ->
            "sat"

        Time.Sun ->
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


displayedWeekDays : List Time.Weekday
displayedWeekDays =
    [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri ]


{-| Used by the decoders
-}
weekdayToNumber : Time.Weekday -> Int
weekdayToNumber weekday =
    case weekday of
        Time.Mon ->
            2

        Time.Tue ->
            3

        Time.Wed ->
            4

        Time.Thu ->
            5

        Time.Fri ->
            6

        Time.Sat ->
            7

        Time.Sun ->
            8


{-| Returns true if a given time is within a start and end date.
Warning: Unable to handle cases where start and end date have different weekdays.
-}
weekTimeIsBetween : WeekTime -> ( WeekTime, WeekTime ) -> Bool
weekTimeIsBetween givenTime ( start, end ) =
    let
        weekdayCond =
            weekdayToNumber givenTime.weekday >= weekdayToNumber start.weekday && weekdayToNumber givenTime.weekday <= weekdayToNumber end.weekday

        startMinutes =
            start.hour * 60 + start.minute

        endMinutes =
            end.hour * 60 + end.minute

        givenMinutes =
            givenTime.hour * 60 + givenTime.minute
    in
    weekdayCond && givenMinutes >= startMinutes && givenMinutes <= endMinutes
