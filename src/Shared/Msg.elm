module Shared.Msg exposing (Msg(..), UpdateType(..))

import Dict exposing (Dict)
import Route.Path
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = GotToken String
    | GotData Data
    | UpdateData
        UpdateType
        (Maybe
            { path : Route.Path.Path
            , query : Dict String String
            , hash : Maybe String
            }
        )


type UpdateType
    = UpdateEvent ( EventID, Event )
    | UpdateRoom ( RoomID, Room )
    | UpdateLect ( LecturerID, Lecturer )
    | UpdateBlock ( BlockID, Block )
    | DeleteEvent EventID
    | DeleteRoom RoomID
    | DeleteLect LecturerID
    | DeleteBlock BlockID
