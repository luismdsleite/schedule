module DragDrop exposing (..)

import DnD
import ScheduleObjects exposing (Event, WeekTime)
import Table exposing (ID)


type alias Draggable =
    DnD.Draggable (DropEvent, WeekTime) ID 


type DropEvent
    = RoomEvent ID
    | LectEvent ID
    | BlockEvent ID
