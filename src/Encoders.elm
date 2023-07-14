module Encoders exposing (..)

import DeployEnv exposing (serverUrl)
import Http
import Json.Encode as Encode
import RenderMain.Msg exposing (Msg(..))
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (weekdayToNumber)


updateEvent : ( EventID, Event ) -> Cmd Msg
updateEvent ( id, event ) =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = serverUrl ++ "events\\" ++ String.fromInt id
        , body = Http.jsonBody (putEvent ( id, event ))
        , expect = Http.expectWhatever (handleResponse ( id, event ))
        , timeout = Nothing
        , tracker = Nothing
        }



-- fetchEvent : Int -> Cmd Msg
-- fetchEvent id =
--     Http.get
--         { url = serverUrl ++ "events\\" ++ String.fromInt id
--         , expect = Http.expectJson UpdateEventResult Decoders.getEventAndID
--         }


{-| TODO: Fix this code
When we update an event there are 2 possible options:

1.  The event is updated successfully, in this case we do a GET request to get the updated event
2.  The event is not updated, in this case we do nothing

-}
handleResponse : ( EventID, Event ) -> Result Http.Error () -> RenderMain.Msg.Msg
handleResponse ( evID, ev ) response =
    case response of
        Ok _ ->
            RenderMain.Msg.UpdateEvent (Ok ( evID, ev ))

        Err err ->
            RenderMain.Msg.UpdateEvent (Err err)


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
