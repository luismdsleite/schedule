module RenderMain.Msg exposing (..)

import DnD
import Http
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type Msg
    = ItemClick OnItemClick
    | DnDMsg (DnD.Msg ( DropEvent, WeekTime ) ID)
    | OnDrop ( DropEvent, WeekTime ) ID
    | UpdateEvent (Result Http.Error ( EventID, Event ))


type OnItemClick
    = OnRoomClick Int
    | OnLecturerClick Int
    | OnEventClick Int
    | OnBlockClick ( Int, Block )
    | ChangeEventRoomClick EventID RoomID


type alias Draggable =
    DnD.Draggable ( DropEvent, WeekTime ) ID


{-| Init Drag and Drop messages
-}
dnd : DnD.DraggableInit ( DropEvent, WeekTime ) ID Msg
dnd =
    DnD.init DnDMsg OnDrop


type DropEvent
    = RoomEvent ID
    | LectEvent ID
    | BlockEvent ID
