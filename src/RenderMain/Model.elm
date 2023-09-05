module RenderMain.Model exposing (Model(..), init)

-- import Dict

import Effect exposing (Effect)
import RenderMain.Msg exposing (Draggable, Msg(..), dnd)
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Filters exposing (ScheduleFilter)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)



-- import ScheduleObjects.Block exposing (Block)
-- import ScheduleObjects.Event exposing (Event)
-- import ScheduleObjects.Lecturer exposing (Lecturer)
-- import ScheduleObjects.Room exposing (Room)
-- import ScheduleObjects.WeekTime exposing (WeekTime)
-- import Time


type Model
    = Model Data ScheduleFilter Draggable SelectedItemsInList


{-| Item (can be either an ev, room, lecturer or a block) selected in the lists
-}
type alias SelectedItemsInList =
    { room : Maybe ( RoomID, Room )
    , lect : Maybe ( LecturerID, Lecturer )
    , event : Maybe ( EventID, Event )
    , block : Maybe ( BlockID, Block )
    }


init : Data -> () -> ( Model, Effect Msg )
init data () =
    ( Model data
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) "" "" "")
        dnd.model
        (SelectedItemsInList Nothing Nothing Nothing Nothing)
    , Effect.none
    )
