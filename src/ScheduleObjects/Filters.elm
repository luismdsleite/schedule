module ScheduleObjects.Filters exposing (..)
import ScheduleObjects.Event exposing (Event)


---- Filters ----


{-| A Filter is a specific view of all events (e.g. show all events from room X or show all events whose lecturer is C).
The ScheduleFilter holds 3 different filter functions that are only parsed when displaying html.
-}
type alias ScheduleFilter =
    { room : Filter, lect : Filter, block : Filter, roomName : String, lectName : String, blockName : String }


type alias Filter =
    Int -> Event -> Bool