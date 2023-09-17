module ScheduleObjects.Data exposing (Data, Token, asDataIn, setData, setDataBlocks, setDataEvents, setDataHiddenBlocks, setDataHiddenEvents, setDataHiddenLect, setDataHiddenRooms, setDataLect, setDataOccupations, setDataRestrictions, setDataRooms)

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


setData : a -> { b | data : a } -> { b | data : a }
setData data a =
    { a | data = data }


asDataIn : { a | data : b } -> b -> { a | data : b }
asDataIn a data =
    { a | data = data }


setDataEvents : Dict EventID Event -> Data -> Data
setDataEvents events data =
    { data | events = events }


setDataRooms : Dict RoomID Room -> Data -> Data
setDataRooms rooms data =
    { data | rooms = rooms }


setDataLect : Dict ID Lecturer -> Data -> Data
setDataLect lecturers data =
    { data | lecturers = lecturers }


setDataBlocks : Dict ID Block -> Data -> Data
setDataBlocks blocks data =
    { data | blocks = blocks }


setDataHiddenRooms : Dict RoomID Room -> Data -> Data
setDataHiddenRooms rooms data =
    { data | hiddenRooms = rooms }


setDataHiddenLect : Dict ID Lecturer -> Data -> Data
setDataHiddenLect lecturers data =
    { data | hiddenLecturers = lecturers }


setDataHiddenEvents : Dict EventID Event -> Data -> Data
setDataHiddenEvents events data =
    { data | hiddenEvents = events }


setDataHiddenBlocks : Dict ID Block -> Data -> Data
setDataHiddenBlocks blocks data =
    { data | hiddenBlocks = blocks }


setDataOccupations : Dict OccupationID Occupation -> Data -> Data
setDataOccupations occupations data =
    { data | occupations = occupations }


setDataRestrictions : Dict RestrictionID Restriction -> Data -> Data
setDataRestrictions restrictions data =
    { data | restrictions = restrictions }
