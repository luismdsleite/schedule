module Decoders exposing (..)

{-| Json Decoders used to interact with the servers REST API
-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDE
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Room exposing (Room)
import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time


{-| blocks USED ONLY FOR DEBUG
-}
blocks =
    Dict.fromList
        [ ( 1, Block "All Events" "All Events" (\_ -> True) )
        , ( 2, Block "Eventos de Alga" "(CC4011)" (\ev -> String.contains "(CC4011)" ev.subject) )
        ]


objectsToDictParser : Decoder a -> Decoder (Dict ID a)
objectsToDictParser objectDecoder =
    (JD.list (JD.map2 Tuple.pair (JD.field "Id" JD.int) objectDecoder)
        |> JD.map Dict.fromList
    )
        |> JD.at [ "data" ]


roomParser : Decoder Room
roomParser =
    JD.map4 Room
        (JD.field "Name" JD.string)
        (JD.field "NameAbbr" JD.string)
        (JD.field "Capacity" JD.int)
        (JD.field "Number" JD.string)


lectParser : Decoder Lecturer
lectParser =
    JD.map6 Lecturer
        (JD.field "Name" JD.string)
        (JD.field "NameAbbr" JD.string)
        (JD.fail "Good Time Not Implemented" |> JDE.withDefault [])
        (JD.fail "Difficult Time Not Implemented" |> JDE.withDefault [])
        (JD.fail "Unavailable Time Not Implemented" |> JDE.withDefault [])
        (JD.field "Office" JD.string)


eventParser : Decoder Event
eventParser =
    JD.map6 Event
        (JD.field "Subject" JD.string)
        (JD.field "SubjectAbbr" JD.string)
        -- (JD.field "RoomId" (Just JD.int) )
        (JD.maybe (JD.field "RoomId" JD.int))
        (JD.maybe (JD.field "LecturerId" JD.int))
        (JD.fail "Start Time Implemented" |> JDE.withDefault (Just (WeekTime Time.Mon 10 30)))
        (JD.fail "End Time Implemented" |> JDE.withDefault (Just (WeekTime Time.Mon 12 30)))
