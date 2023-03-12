module ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.Lecturer exposing (LecturerID)

type alias Event =
    { subject : String
    , subjectAbbr : String
    , room : Maybe RoomID
    , lecturer : Maybe LecturerID
    , start_time : Maybe WeekTime
    , end_time : Maybe WeekTime
    }

type alias EventID =
    ID