module ScheduleObjects.Filters exposing (..)

import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)



---- Filters ----


{-| A Filter is a specific view of all events (e.g. show all events from room X or show all events whose lecturer is C).
The ScheduleFilter holds 3 different filter functions that are only parsed when displaying html.
-}
type alias ScheduleFilter =
    { room : Filter, lect : Filter, block : Filter, occupations : OccupationsFilter, restrictions : RestrictionsFilter }


setRoomFilter : Filter -> ScheduleFilter -> ScheduleFilter
setRoomFilter f sf =
    { sf | room = f }


setLectFilter : Filter -> ScheduleFilter -> ScheduleFilter
setLectFilter f sf =
    { sf | lect = f }


setBlockFilter : Filter -> ScheduleFilter -> ScheduleFilter
setBlockFilter f sf =
    { sf | block = f }


setOccFilter : OccupationsFilter -> ScheduleFilter -> ScheduleFilter
setOccFilter f sf =
    { sf | occupations = f }


setRestFilter : RestrictionsFilter -> ScheduleFilter -> ScheduleFilter
setRestFilter f sf =
    { sf | restrictions = f }


type alias Filter =
    EventID -> Event -> Bool


type alias OccupationsFilter =
    OccupationID -> Occupation -> Bool


type alias RestrictionsFilter =
    RestrictionID -> Restriction -> Bool
