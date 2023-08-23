module ScheduleObjects.Filters exposing (..)

import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)



---- Filters ----


{-| A Filter is a specific view of all events (e.g. show all events from room X or show all events whose lecturer is C).
The ScheduleFilter holds 3 different filter functions that are only parsed when displaying html.
-}
type alias ScheduleFilter =
    { room : Filter, lect : Filter, block : Filter, occupations : OccupationsFilter, restrictions : RestrictionsFilter, roomName : String, lectName : String, blockName : String }


type alias Filter =
    EventID -> Event -> Bool


type alias OccupationsFilter =
    OccupationID -> Occupation -> Bool


type alias RestrictionsFilter =
    RestrictionID -> Restriction -> Bool
