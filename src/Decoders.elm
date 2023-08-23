module Decoders exposing (blockParser, eventParser, lectParser, objectsToDictParser, occupationParser, restrictionParser, roomParser)

{-| Json Decoders used to interact with the servers REST API
-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDE
import List
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Occupation exposing (Occupation)
import ScheduleObjects.Restriction exposing (Restriction)
import ScheduleObjects.Room exposing (Room)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time


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


getBlockAndId : Decoder ( ID, Block )
getBlockAndId =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) blockParser


blockParser : Decoder Block
blockParser =
    JD.map3 Block
        (JD.field "Name" JD.string)
        (JD.field "NameAbbr" JD.string)
        (JD.field "AssociatedEventIds"
            (JD.list JD.int
                |> JD.map
                    (\eventIds ->
                        \x _ -> List.member x eventIds
                    )
            )
        )


getEventAndID : Decoder ( EventID, Event )
getEventAndID =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) eventParser


{-| Event Decoder
-}
eventParser : Decoder Event
eventParser =
    JD.map6 Event
        (JD.field "Subject" JD.string)
        (JD.field "SubjectAbbr" JD.string)
        -- (JD.field "RoomId" (Just JD.int) )
        (JD.maybe (JD.field "RoomId" JD.int))
        (JD.maybe (JD.field "LecturerId" JD.int))
        (JD.maybe (weektimeDecoder "StartTime"))
        (JD.maybe (weektimeDecoder "EndTime"))


{-| Occupation Decoder
-}
occupationParser : Decoder Occupation
occupationParser =
    JD.map3 Occupation
        (JD.field "RoomId" JD.int)
        (weektimeDecoder "StartTime")
        (weektimeDecoder "EndTime")


{-| Restriction Decoder
-}
restrictionParser : Decoder Restriction
restrictionParser =
    JD.map4 Restriction
        (JD.field "LecturerId" JD.int)
        (weektimeDecoder "StartTime")
        (weektimeDecoder "EndTime")
        (JD.field "Type" JD.int)


{-| Weektime Decoder.
timeToRead == "StartTime" or "EndTime"
-}
weektimeDecoder : String -> Decoder WeekTime
weektimeDecoder timeToRead =
    JD.map3 WeekTime
        (JD.field "WeekDay" weekdayDecoder)
        (JD.field timeToRead hourDecoder)
        (JD.field timeToRead minuteDecoder)


weekdayDecoder : Decoder Time.Weekday
weekdayDecoder =
    JD.int
        |> JD.andThen
            (\n ->
                case n of
                    2 ->
                        JD.succeed Time.Mon

                    3 ->
                        JD.succeed Time.Tue

                    4 ->
                        JD.succeed Time.Wed

                    5 ->
                        JD.succeed Time.Thu

                    6 ->
                        JD.succeed Time.Fri

                    7 ->
                        JD.succeed Time.Sat

                    8 ->
                        JD.succeed Time.Sun

                    _ ->
                        JD.fail "Not Implemented"
            )


hourDecoder : Decoder Int
hourDecoder =
    JD.string
        |> JD.andThen
            (\timeString ->
                case String.split ":" timeString of
                    [ hourStr, _ ] ->
                        JD.succeed
                            (String.toInt hourStr |> Maybe.withDefault 0)

                    _ ->
                        JD.fail "Invalid time format, expected HH:MM"
            )


minuteDecoder : Decoder Int
minuteDecoder =
    JD.string
        |> JD.andThen
            (\timeString ->
                case String.split ":" timeString of
                    [ _, minuteStr ] ->
                        case String.toInt minuteStr of
                            Nothing ->
                                Debug.log "1" JD.fail "Invalid time format, expected HH:MM"

                            Just int ->
                                JD.succeed int

                    _ ->
                        JD.fail <| Debug.log "2" "Invalid time format, expected HH:MM"
            )


blockDecoder : Block -> Decoder Block
blockDecoder =
    JD.succeed
