module ScheduleObjects.Block exposing (..)

import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)


{-| A schedule here is designated as a block.
A block is composed of events. An event is comprised of a subject, given in a specific room by a lecturer during a given time.
-}
type alias Block =
    { name : String, nameAbbr : String, cond : EventID -> Event -> Bool }


type alias BlockID =
    ID


setBlockCond : (EventID -> Event -> Bool) -> Block -> Block
setBlockCond cond block =
    { block | cond = cond }


setBlockAbbr : String -> Block -> Block
setBlockAbbr abbr block =
    { block | nameAbbr = abbr }


setBlockName : String -> Block -> Block
setBlockName name block =
    { block | name = name }
