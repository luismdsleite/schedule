module ScheduleObjects.Occupation exposing (Occupation, OccupationID, setOccEndTime, setOccRoom, setOccStartTime, setOccupation)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Occupation =
    { room : RoomID
    , start_time : WeekTime
    , end_time : WeekTime
    }


setOccupation : Occupation -> { a | occupation : Occupation } -> { a | occupation : Occupation }
setOccupation occupation a =
    { a | occupation = occupation }


setOccStartTime : WeekTime -> Occupation -> Occupation
setOccStartTime new_start_time occupation =
    { occupation | start_time = new_start_time }


setOccEndTime : WeekTime -> Occupation -> Occupation
setOccEndTime new_end_time occupation =
    { occupation | end_time = new_end_time }


setOccRoom : RoomID -> Occupation -> Occupation
setOccRoom new_room occupation =
    { occupation | room = new_room }


type alias OccupationID =
    ID
