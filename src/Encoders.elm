module Encoders exposing (..)

import DeployEnv exposing (serverUrl)
import Http
import Json.Encode as Encode
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (weekdayToNumber)


putRoom : ( RoomID, Room ) -> Encode.Value
putRoom ( id, room ) =
    Encode.object
        [ ( "Id", Encode.int id )
        , ( "Hide", Encode.int 0 )
        , ( "Name", Encode.string room.name )
        , ( "NameAbbr", Encode.string room.abbr )
        , ( "Number", Encode.string room.number )
        ]


putLecturer : ( LecturerID, Lecturer ) -> Encode.Value
putLecturer ( id, lect ) =
    Encode.object
        [ ( "Id", Encode.int id )
        , ( "Hide", Encode.int 0 )
        , ( "Name", Encode.string lect.name )
        , ( "NameAbbr", Encode.string lect.abbr )
        , ( "Number", Encode.string lect.office )
        ]


putEvent : ( EventID, Event ) -> Encode.Value
putEvent ( id, event ) =
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
                    Encode.string (String.fromInt v.hour ++ ":" ++ String.fromInt v.minute)

                Nothing ->
                    Encode.null
    in
    Encode.object
        [ ( "Id", Encode.int id )
        , ( "Hide", Encode.int 0 )
        , ( "RoomId", nullInCaseOfNothing Encode.int event.room )
        , ( "LecturerId", nullInCaseOfNothing Encode.int event.lecturer )
        , ( "Subject", Encode.string event.subject )
        , ( "SubjectAbbr", Encode.string event.subjectAbbr )
        , ( "StartTime", convertToTime event.start_time )
        , ( "EndTime", convertToTime event.end_time )
        , ( "WeekDay", weekDay )
        ]


login : String -> String -> Encode.Value
login username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]
