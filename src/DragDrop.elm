module DragDrop exposing (..)

import DnD
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.Id exposing (ID)


type alias Draggable =
    DnD.Draggable ( DropEvent, WeekTime ) ID


type DropEvent
    = RoomEvent ID
    | LectEvent ID
    | BlockEvent ID
