module Decoders exposing (blockParser, errorToString, eventParser, getBlockAndID, getEventAndID, getLectAndID, getOccupationAndId, getRestrictionAndId, getRoomAndID, lectParser, objectsToDictParser, occupationParser, responseParser, restrictionParser, roomParser, tokenParser)

{-| Json Decoders used to interact with the servers REST API
-}

import Dict exposing (Dict)
import Http
import Json.Decode as JD exposing (Decoder)
import List
import Maybe.Extra
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction as Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time


objectsToDictParser : Decoder a -> Decoder (Dict ID a)
objectsToDictParser objectDecoder =
    (JD.list (JD.map2 Tuple.pair (JD.field "Id" JD.int) objectDecoder)
        |> JD.map Dict.fromList
    )
        |> JD.at [ "data" ]


responseParser : Decoder a -> Decoder a
responseParser decoder =
    JD.field "data" decoder


addHideProperty : Decoder a -> Decoder ( a, Bool )
addHideProperty =
    JD.andThen
        (\obj ->
            JD.field "Hide" JD.int
                |> JD.andThen
                    (\n ->
                        if n == 0 then
                            JD.succeed False

                        else
                            JD.succeed True
                    )
                |> JD.map (\isHidden -> ( obj, isHidden ))
        )


getBlockAndID : Decoder ( ID, ( Block, IsHidden ) )
getBlockAndID =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) blockParser


getEventAndID : Decoder ( EventID, ( Event, IsHidden ) )
getEventAndID =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) eventParser


getRoomAndID : Decoder ( RoomID, ( Room, IsHidden ) )
getRoomAndID =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) roomParser


getLectAndID : Decoder ( LecturerID, ( Lecturer, IsHidden ) )
getLectAndID =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) lectParser


getRestrictionAndId : Decoder ( RestrictionID, Restriction )
getRestrictionAndId =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) restrictionParser


getOccupationAndId : Decoder ( OccupationID, Occupation )
getOccupationAndId =
    JD.map2 Tuple.pair (JD.field "Id" JD.int) occupationParser


roomParser : Decoder ( Room, IsHidden )
roomParser =
    JD.map4 Room
        (JD.field "Name" JD.string)
        (JD.field "NameAbbr" JD.string)
        (JD.field "Capacity" JD.int)
        (JD.field "Number" JD.string)
        |> addHideProperty


lectParser : Decoder ( Lecturer, IsHidden )
lectParser =
    JD.map3 Lecturer
        (JD.field "Name" JD.string)
        (JD.field "NameAbbr" JD.string)
        (JD.field "Office" JD.string)
        |> addHideProperty


blockParser : Decoder ( Block, IsHidden )
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
        |> addHideProperty


{-| Event Decoder
-}
eventParser : Decoder ( Event, IsHidden )
eventParser =
    JD.map6 Event
        (JD.field "Subject" JD.string)
        (JD.field "SubjectAbbr" JD.string)
        (JD.maybe (JD.field "RoomId" JD.int))
        (JD.maybe (JD.field "LecturerId" JD.int))
        (JD.maybe (weektimeDecoder "StartTime"))
        (JD.maybe (weektimeDecoder "EndTime"))
        |> addHideProperty


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
    let
        categoryParser =
            JD.int
                |> JD.andThen
                    (\n ->
                        case n of
                            0 ->
                                JD.succeed Restriction.Preference

                            1 ->
                                JD.succeed Restriction.Service

                            2 ->
                                JD.succeed Restriction.Priority

                            _ ->
                                JD.succeed Restriction.Other
                    )
    in
    JD.map4 Restriction
        (JD.field "LecturerId" JD.int)
        (weektimeDecoder "StartTime")
        (weektimeDecoder "EndTime")
        (JD.field "Type" categoryParser)


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
                            (String.toInt hourStr |> Maybe.Extra.withDefaultLazy (\() -> 0))

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
                                JD.fail "Invalid time format, expected HH:MM"

                            Just int ->
                                JD.succeed int

                    _ ->
                        JD.fail <| "Invalid time format, expected HH:MM"
            )


tokenParser : Decoder String
tokenParser =
    JD.field "access_token" JD.string


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "URL " ++ url ++ " é invalido"

        Http.Timeout ->
            "Não foi possivel contactar o servidor, tente mais tarde"

        Http.NetworkError ->
            "Não foi possivel contactar o servidor, verifique a sua ligação à internet"

        Http.BadStatus 500 ->
            "O servidor teve um problema, tente mais tarde"

        Http.BadStatus 400 ->
            "Verifique a sua informação e tente novamente"

        Http.BadStatus int ->
            "Erro Desconhecido (" ++ String.fromInt int ++ ")"

        Http.BadBody errorMessage ->
            errorMessage
