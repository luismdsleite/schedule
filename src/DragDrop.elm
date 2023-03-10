module DragDrop exposing (..)

import DnD
import ScheduleObjects exposing (ID, WeekTime)


type alias Draggable =
    DnD.Draggable ( DropEvent, WeekTime ) ID


type DropEvent
    = RoomEvent ID
    | LectEvent ID
    | BlockEvent ID
