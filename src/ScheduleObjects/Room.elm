module ScheduleObjects.Room exposing (Room, RoomID)

{-| A room has an ID, a name, a abbreviation and a capacity.
-}
import ScheduleObjects.Id exposing (ID)
type alias Room =
    { name : String, abbr : String, capacity : Int, number : String }


type alias RoomID =
    ID
