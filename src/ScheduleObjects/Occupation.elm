module ScheduleObjects.Occupation exposing (Occupation, OccupationID)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Occupation =
    { room : RoomID
    , start_time : WeekTime
    , end_time : WeekTime
    }


type alias OccupationID =
    ID
