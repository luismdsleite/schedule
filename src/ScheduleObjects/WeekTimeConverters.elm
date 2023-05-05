module ScheduleObjects.WeekTimeConverters exposing (..)
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.Event exposing (Event)
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

{-| Used by the decoders-}
weekdayToNumber : Time.Weekday -> Int
weekdayToNumber weekday =
    case weekday of
        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6

        Time.Sun ->
            7
