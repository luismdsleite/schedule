module Main.Msg exposing (..)

import DnD
import Http
import Route exposing (Route)
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type Msg
    = ItemClick OnItemClick
    | DnDMsg (DnD.Msg ( DropEvent, WeekTime ) ID)
    | OnDrop ( DropEvent, WeekTime ) ID
    | UpdateEvent (Result Http.Error ( EventID, ( Event, IsHidden ) ))
    | EditMenu EditMenu


type OnItemClick
    = OnRoomClick ( RoomID, Room )
    | OnLecturerClick ( LecturerID, Lecturer )
    | OnEventClick ( EventID, Event )
    | OnBlockClick ( BlockID, Block )
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


type EditMenu
    = EditEvent EventID
    | AddEvent
    | EditRoom RoomID
    | AddRoom
    | EditLect LecturerID
    | AddLect
    | EditBlock BlockID
    | AddBlock
