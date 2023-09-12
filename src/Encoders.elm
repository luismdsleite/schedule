module Encoders exposing (login, putBlock, putEvent, putLecturer, putRestriction, putRoom)

import Dict exposing (Dict)
import Json.Encode as Encode
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Restriction as Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (convertHourAndMinute, weekdayToNumber)
import Tuple


idProperty : Maybe ID -> List ( String, Encode.Value )
idProperty maybeId =
    case maybeId of
        Just id ->
            [ ( "Id", Encode.int id ) ]

        Nothing ->
            []


{-| Encoder for a Room. Can optionally also encode a RoomID
-}
putRoom : Maybe RoomID -> Room -> Encode.Value
putRoom maybeId room =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", Encode.int 0 )
               , ( "Name", Encode.string room.name )
               , ( "NameAbbr", Encode.string room.abbr )
               , ( "Capacity", Encode.int room.capacity )
               , ( "Number", Encode.string room.number )
               ]


{-| Encoder for a Block. Can optionally also encode a BlockID
-}
putBlock : Maybe ID -> Dict EventID Event -> Block -> Encode.Value
putBlock maybeId events block =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", Encode.int 0 )
               , ( "Name", Encode.string block.name )
               , ( "NameAbbr", Encode.string block.nameAbbr )
               , ( "AssociatedEventIds", Encode.list Encode.int (Dict.filter block.cond events |> Dict.toList |> List.map Tuple.first) )
               ]


{-| Encoder for a Lecturer. Can optionally also encode a LecturerID
-}
putLecturer : Maybe LecturerID -> Lecturer -> Encode.Value
putLecturer maybeId lect =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", Encode.int 0 )
               , ( "Name", Encode.string lect.name )
               , ( "NameAbbr", Encode.string lect.abbr )
               , ( "Office", Encode.string lect.office )
               ]


{-| Encoder for an Event. Can optionally also encode an EventID
-}
putEvent : Maybe EventID -> Event -> Encode.Value
putEvent maybeId event =
    let
        nullInCaseOfNothing funct maybe =
            case maybe of
                Just v ->
                    funct v

                Nothing ->
                    Encode.null

        weekDay =
            case event.start_time of
                Just v ->
                    Encode.int (weekdayToNumber v.weekday)

                Nothing ->
                    Encode.null

        convertToTime time =
            case time of
                Just v ->
                    Encode.string (convertHourAndMinute v.hour v.minute)

                Nothing ->
                    Encode.null
    in
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", Encode.int 0 )
               , ( "RoomId", nullInCaseOfNothing Encode.int event.room )
               , ( "LecturerId", nullInCaseOfNothing Encode.int event.lecturer )
               , ( "Subject", Encode.string event.subject )
               , ( "SubjectAbbr", Encode.string event.subjectAbbr )
               , ( "StartTime", convertToTime event.start_time )
               , ( "EndTime", convertToTime event.end_time )
               , ( "WeekDay", weekDay )
               ]


putRestriction : Maybe RestrictionID -> Restriction -> Encode.Value
putRestriction maybeId restriction =
    let
        weekDay =
            Encode.int (weekdayToNumber restriction.end_time.weekday)

        convertToTime time =
            Encode.string (convertHourAndMinute time.hour time.minute)

        categoryParser category =
            case category of
                Restriction.Preference ->
                    Encode.int 0

                Restriction.Service ->
                    Encode.int 1

                Restriction.Priority ->
                    Encode.int 2

                Restriction.Other ->
                    Encode.int 3
    in
    Encode.object <|
        idProperty maybeId
            ++ [ ( "LecturerId", Encode.int restriction.lect )
               , ( "StartTime", convertToTime restriction.start_time )
               , ( "EndTime", convertToTime restriction.end_time )
               , ( "WeekDay", weekDay )
               , ( "Type", categoryParser restriction.category )
               ]


login : String -> String -> Encode.Value
login username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]
