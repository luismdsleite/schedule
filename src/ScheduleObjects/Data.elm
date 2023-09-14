module ScheduleObjects.Data exposing (Data, Token)

import Dict exposing (Dict)
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.Room exposing (Room, RoomID)


type alias Token =
    String


type alias Data =
    { rooms : Dict RoomID Room
    , lecturers : Dict ID Lecturer
    , events : Dict EventID Event
    , blocks : Dict ID Block
    , hiddenRooms : Dict RoomID Room
    , hiddenLecturers : Dict ID Lecturer
    , hiddenEvents : Dict EventID Event
    , hiddenBlocks : Dict ID Block
    , occupations : Dict OccupationID Occupation
    , restrictions : Dict RestrictionID Restriction
    , token : Token
    , backendUrl : String
    }
