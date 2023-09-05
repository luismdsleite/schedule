module Encoders exposing (login, putEvent, putLecturer, putRoom)

import Json.Encode as Encode
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (weekdayToNumber)


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
               , ( "Number", Encode.string room.number )
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
               , ( "Number", Encode.string lect.office )
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
                    Encode.string (String.fromInt v.hour ++ ":" ++ String.fromInt v.minute)

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


login : String -> String -> Encode.Value
login username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]
