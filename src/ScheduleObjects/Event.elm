module ScheduleObjects.Event exposing (Event, EventID, asEventIn, setEvent, setEventAbbr, setEventEndTime, setEventLecturer, setEventRoom, setEventStartTime, setEventSubject)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Event =
    { subject : String
    , subjectAbbr : String
    , room : Maybe RoomID
    , lecturer : Maybe LecturerID
    , start_time : Maybe WeekTime
    , end_time : Maybe WeekTime
    }


type alias EventID =
    ID


setEvent : Event -> { b | event : Event } -> { b | event : Event }
setEvent event a =
    { a | event = event }


asEventIn : { b | event : Event } -> Event -> { b | event : Event }
asEventIn a event =
    { a | event = event }


setEventSubject : String -> Event -> Event
setEventSubject subject event =
    { event | subject = subject }


setEventAbbr : String -> Event -> Event
setEventAbbr abbr event =
    { event | subjectAbbr = abbr }


setEventLecturer : Maybe LecturerID -> Event -> Event
setEventLecturer maybeLectID event =
    { event | lecturer = maybeLectID }


setEventRoom : Maybe RoomID -> Event -> Event
setEventRoom maybeRoomID event =
    { event | room = maybeRoomID }


setEventStartTime : Maybe WeekTime -> Event -> Event
setEventStartTime maybeStart event =
    { event | start_time = maybeStart }


setEventEndTime : Maybe WeekTime -> Event -> Event
setEventEndTime maybeEnd event =
    { event | end_time = maybeEnd }
