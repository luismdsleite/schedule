module ScheduleObjects.Restriction exposing (Restriction, RestrictionID)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Restriction =
    { lect : LecturerID
    , start_time : WeekTime
    , end_time : WeekTime
    , category : Int
    }


type alias RestrictionID =
    ID
