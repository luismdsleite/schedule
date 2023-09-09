module ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)

{-| A lecturer/teacher has an ID, a name, a abbreviation, times of availibility and a office.
-}

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Lecturer =
    { name : String, abbr : String, office : String }


type alias LecturerID =
    ID
