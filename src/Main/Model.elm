module Main.Model exposing (Model, init, setFilters, setSelectedBlock, setSelectedEvent, setSelectedItems, setSelectedLect, setSelectedRoom)

-- import Dict

import Effect exposing (Effect)
import Main.Msg exposing (Draggable, Msg(..), dnd)
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Filters exposing (ScheduleFilter)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)


type alias Model =
    { data : Data
    , filters : ScheduleFilter
    , draggable : Draggable
    , selectedItems : SelectedItemsInList
    }


setSelectedItems : SelectedItemsInList -> { b | selectedItems : SelectedItemsInList } -> { b | selectedItems : SelectedItemsInList }
setSelectedItems selectedItems a =
    { a | selectedItems = selectedItems }


setFilters : ScheduleFilter -> { b | filters : ScheduleFilter } -> { b | filters : ScheduleFilter }
setFilters filters a =
    { a | filters = filters }


{-| Item (can be either an ev, room, lecturer or a block) selected in the lists
-}
type alias SelectedItemsInList =
    { room : Maybe ( RoomID, Room )
    , lect : Maybe ( LecturerID, Lecturer )
    , event : Maybe ( EventID, Event )
    , block : Maybe ( BlockID, Block )
    }


setSelectedRoom : Maybe ( RoomID, Room ) -> SelectedItemsInList -> SelectedItemsInList
setSelectedRoom room selectedItems =
    { selectedItems | room = room }


setSelectedLect : Maybe ( LecturerID, Lecturer ) -> SelectedItemsInList -> SelectedItemsInList
setSelectedLect lect selectedItems =
    { selectedItems | lect = lect }


setSelectedEvent : Maybe ( EventID, Event ) -> SelectedItemsInList -> SelectedItemsInList
setSelectedEvent event selectedItems =
    { selectedItems | event = event }


setSelectedBlock : Maybe ( BlockID, Block ) -> SelectedItemsInList -> SelectedItemsInList
setSelectedBlock block selectedItems =
    { selectedItems | block = block }


init : Data -> () -> ( Model, Effect Msg )
init data () =
    ( Model data
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False))
        dnd.model
        (SelectedItemsInList Nothing Nothing Nothing Nothing)
    , Effect.none
    )
