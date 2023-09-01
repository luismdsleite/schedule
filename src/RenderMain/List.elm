module RenderMain.List exposing (renderAvailableRooms, renderBlocks, renderEvents, renderLecturers, renderRooms)

{-| Responsible for displaying a list of a certain resource (e.g. list of rooms).
-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import RenderMain.Msg exposing (Msg(..), OnItemClick(..))
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Occupation exposing (Occupation)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.WeekTimeConverters exposing (..)


eventTupleComparator : ( Int, Event ) -> ( Int, Event ) -> Order
eventTupleComparator ( _, event1 ) ( _, event2 ) =
    compare event1.subjectAbbr event2.subjectAbbr


roomTupleComparator : ( Int, Room ) -> ( Int, Room ) -> Order
roomTupleComparator ( _, room1 ) ( _, room2 ) =
    compare room1.abbr room2.abbr


lectTupleComparator : ( Int, Lecturer ) -> ( Int, Lecturer ) -> Order
lectTupleComparator ( _, lect1 ) ( _, lect2 ) =
    compare lect1.abbr lect2.abbr


blockTupleComparator : ( Int, Block ) -> ( Int, Block ) -> Order
blockTupleComparator ( _, block1 ) ( _, block2 ) =
    compare block1.nameAbbr block2.nameAbbr


{-| Renders all the events into a list
-}
renderEvents : List ( Int, Event ) -> Dict RoomID Room -> Dict LecturerID Lecturer -> Html Msg
renderEvents events rooms lecturers =
    ul [ ariaLabel "Cadeiras", class "list custom-scrollbar" ]
        (List.map (renderEvent rooms lecturers) (List.sortWith eventTupleComparator events))


{-| Transforms an event into a list item
-}
renderEvent : Dict RoomID Room -> Dict LecturerID Lecturer -> ( Int, Event ) -> Html Msg
renderEvent rooms lecturers ( eventID, event ) =
    let
        room =
            case event.room of
                Just roomID ->
                    case Dict.get roomID rooms of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Room "----" "----" -1 "----"

                -- Event still has no room assigned
                Nothing ->
                    Room "----" "----" -1 "----"

        lecturer =
            case event.lecturer of
                Just lecturerID ->
                    case Dict.get lecturerID lecturers of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Lecturer "----" "----" [] [] [] ""

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" [] [] [] ""
    in
    li [ class "list-item", onClick (ItemClick (OnEventClick eventID)) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%", attribute "title" event.subjectAbbr ] [ text event.subjectAbbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "35%", attribute "title" event.subject ] [ text event.subject ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "5%" ] [ text (convertWeekDay event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertWeekTimeHourAndMinute event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertWeekTimeHourAndMinute event.end_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%", attribute "title" room.abbr ] [ text room.abbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (String.fromInt room.capacity) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text lecturer.abbr ]
        ]


renderRooms : Dict RoomID Room -> Html Msg
renderRooms rooms =
    let
        roomsList =
            Dict.toList rooms |> List.sortWith roomTupleComparator
    in
    ul [ ariaLabel "Salas", class "list custom-scrollbar" ]
        (List.map renderRoom roomsList)


renderRoom : ( Int, Room ) -> Html Msg
renderRoom ( int, room ) =
    li [ class "list-item", onClick (ItemClick (OnRoomClick int)), attribute "title" room.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


renderLecturers : Dict LecturerID Lecturer -> Html Msg
renderLecturers lecturers =
    let
        lecturersList =
            Dict.toList lecturers |> List.sortWith lectTupleComparator
    in
    ul [ ariaLabel "Docentes", class "list custom-scrollbar" ]
        (List.map renderLecturer lecturersList)


renderLecturer : ( Int, Lecturer ) -> Html Msg
renderLecturer ( int, lecturer ) =
    li [ class "list-item", onClick (ItemClick (OnLecturerClick int)), attribute "title" lecturer.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]


renderBlocks : Dict BlockID Block -> Html Msg
renderBlocks blocks =
    let
        blocksList =
            Dict.toList blocks |> List.sortWith blockTupleComparator
    in
    ul [ ariaLabel "Blocos", class "list custom-scrollbar" ] (List.map renderBlock blocksList)


renderBlock : ( Int, Block ) -> Html Msg
renderBlock ( id, block ) =
    li [ class "list-item", onClick (ItemClick (OnBlockClick ( id, block ))), attribute "title" block.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text block.nameAbbr ] ]


{-| Given a certain event, a list of all rooms and a list of all events, it returns what rooms are available to host that event.
-}
renderAvailableRooms : ( EventID, Event ) -> Dict RoomID Room -> List Event -> List Occupation -> Html Msg
renderAvailableRooms ( eventId, event ) rooms events occupations =
    let
        timeslots =
            case event.start_time of
                Just start ->
                    case event.end_time of
                        Just end ->
                            computeTimeSlots start end []

                        -- ERROR: Event has no end time!
                        Nothing ->
                            []

                -- ERROR: Event has no start time!
                Nothing ->
                    []

        isRoomAvailable : RoomID -> Room -> Bool
        isRoomAvailable roomId _ =
            let
                eventsOfRoom : List Event
                eventsOfRoom =
                    List.filter (\ev -> ev.room == Just roomId) events

                occupationsOfRoom : List Occupation
                occupationsOfRoom =
                    List.filter (\occ -> occ.room == roomId) occupations

                -- Checks if a certain time slot is occupied by an event or an occupation
                isTimeSlotOccupied : WeekTime -> Bool
                isTimeSlotOccupied tslot =
                    List.any
                        (\ev ->
                            case ev.start_time of
                                Just start ->
                                    case ev.end_time of
                                        Just end ->
                                            weekTimeIsBetween tslot ( start, end )

                                        -- ERROR: Event has no end time!
                                        Nothing ->
                                            False

                                -- ERROR: Event has no start time!
                                Nothing ->
                                    False
                        )
                        eventsOfRoom
                        || List.any
                            (\occ -> weekTimeIsBetween tslot ( occ.start_time, occ.end_time ))
                            occupationsOfRoom
            in
            List.all (not << isTimeSlotOccupied) timeslots

        availableRooms =
            Dict.filter isRoomAvailable rooms |> Dict.toList |> List.sortWith roomTupleComparator
    in
    ul [ ariaLabel ("Salas Lives para " ++ event.subjectAbbr), class "list custom-scrollbar" ]
        (List.map (renderAvailableRoom eventId) availableRooms)


renderAvailableRoom : EventID -> ( RoomID, Room ) -> Html Msg
renderAvailableRoom evId ( roomId, room ) =
    li [ class "list-item", onClick (ItemClick (ChangeEventRoomClick evId roomId)), attribute "title" room.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]
