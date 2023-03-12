module ScheduleObjects.WeekTime exposing (WeekTime)
import Time

type alias WeekTime =
    { weekday : Time.Weekday, hour : Int, minute : Int }

