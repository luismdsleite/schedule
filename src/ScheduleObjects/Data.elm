module ScheduleObjects.Data exposing (Data)

import Dict exposing (Dict)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.Event exposing (Event,EventID)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Block exposing (Block)

type alias Data =
    { rooms : Dict RoomID Room
    , lecturers : Dict ID Lecturer
    , events : Dict EventID Event
    , blocks : Dict ID Block
    }


