module Encoders exposing (login, putBlock, putEvent, putLecturer, putOccupation, putRestriction, putRoom)

import Dict exposing (Dict)
import Json.Encode as Encode
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Occupation as Occupation exposing (Occupation, OccupationID)
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


hideEncoder : IsHidden -> Encode.Value
hideEncoder isHidden =
    case isHidden of
        True ->
            Encode.int 1

        False ->
            Encode.int 0


{-| Encoder for a Room. Can optionally also encode a RoomID
-}
putRoom : Maybe RoomID -> Room -> IsHidden -> Encode.Value
putRoom maybeId room isHidden =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", hideEncoder isHidden )
               , ( "Name", Encode.string room.name )
               , ( "NameAbbr", Encode.string room.abbr )
               , ( "Capacity", Encode.int room.capacity )
               , ( "Number", Encode.string room.number )
               ]


{-| Encoder for a Block. Can optionally also encode a BlockID
-}
putBlock : Maybe ID -> Dict EventID Event -> Block -> IsHidden -> Encode.Value
putBlock maybeId events block isHidden =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", hideEncoder isHidden )
               , ( "Name", Encode.string block.name )
               , ( "NameAbbr", Encode.string block.nameAbbr )
               , ( "AssociatedEventIds", Encode.list Encode.int (Dict.filter block.cond events |> Dict.toList |> List.map Tuple.first) )
               ]


{-| Encoder for a Lecturer. Can optionally also encode a LecturerID
-}
putLecturer : Maybe LecturerID -> Lecturer -> IsHidden -> Encode.Value
putLecturer maybeId lect isHidden =
    Encode.object <|
        idProperty maybeId
            ++ [ ( "Hide", hideEncoder isHidden )
               , ( "Name", Encode.string lect.name )
               , ( "NameAbbr", Encode.string lect.abbr )
               , ( "Office", Encode.string lect.office )
               ]


{-| Encoder for an Event. Can optionally also encode an EventID
-}
putEvent : Maybe EventID -> Event -> IsHidden -> Encode.Value
putEvent maybeId event isHidden =
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
            ++ [ ( "Hide", hideEncoder isHidden )
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
            Encode.int (weekdayToNumber restriction.start_time.weekday)

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


putOccupation : Maybe OccupationID -> Occupation -> Encode.Value
putOccupation maybeId occupation =
    let
        weekDay =
            Encode.int (weekdayToNumber occupation.start_time.weekday)

        convertToTime time =
            Encode.string (convertHourAndMinute time.hour time.minute)
    in
    Encode.object <|
        idProperty maybeId
            ++ [ ( "RoomId", Encode.int occupation.room )
               , ( "StartTime", convertToTime occupation.start_time )
               , ( "EndTime", convertToTime occupation.end_time )
               , ( "WeekDay", weekDay )
               ]


login : String -> String -> Encode.Value
login username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]
