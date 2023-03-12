module ScheduleObjects.Block exposing (..)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Event exposing (Event)

{-| A schedule here is designated as a block.
A block is composed of events. An event is comprised of a subject, given in a specific room by a lecturer during a given time.
-}
type alias Block =
    { name : String, nameAbbr : String, cond : Event -> Bool }

type alias BlockID =
    ID
