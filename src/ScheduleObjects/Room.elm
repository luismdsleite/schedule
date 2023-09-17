module ScheduleObjects.Room exposing (Room, RoomID, asRoomIn, setRoom, setRoomAbbr, setRoomCapacity, setRoomName, setRoomNumber)

{-| A room has an ID, a name, a abbreviation and a capacity.
-}

import ScheduleObjects.Id exposing (ID)


type alias Room =
    { name : String, abbr : String, capacity : Int, number : String }


type alias RoomID =
    ID


setRoom : { a | a : b } -> b -> { a | a : b }
setRoom room a =
    { room | a = a }


asRoomIn : { a | room : b } -> b -> { a | room : b }
asRoomIn a room =
    { a | room = room }


setRoomName : String -> Room -> Room
setRoomName name room =
    { room | name = name }


setRoomAbbr : String -> Room -> Room
setRoomAbbr abbr room =
    { room | abbr = abbr }


setRoomCapacity : Int -> Room -> Room
setRoomCapacity capacity room =
    { room | capacity = capacity }


setRoomNumber : String -> Room -> Room
setRoomNumber number room =
    { room | number = number }
