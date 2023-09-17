module Main.List exposing (renderAvailableRooms, renderBlocks, renderEvents, renderLecturers, renderRooms)

{-| Responsible for displaying a list of a certain resource (e.g. list of rooms).
-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Main.Msg exposing (EditMenu(..), Msg(..), OnItemClick(..))
import Maybe.Extra exposing (isJust)
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


{-| Renders all the events into a list.
-}
renderEvents : List ( Int, Event ) -> List ( Int, Event ) -> Dict RoomID Room -> Dict LecturerID Lecturer -> Maybe ( EventID, Event ) -> Html Msg
renderEvents events hiddenEvents rooms lecturers selectedEvent =
    let
        modifyIcon =
            case selectedEvent of
                Just ( id, _ ) ->
                    div [ class "gg-pen", onClick (EditMenu (EditEvent id)) ] []

                Nothing ->
                    div [ style "display" "none" ] []
    in
    ul [ class "list custom-scrollbar" ]
        (ul [ ariaLabel "Cadeiras", class "list-title" ] [ modifyIcon, div [ class "gg-add", onClick (EditMenu AddEvent) ] [] ] :: List.map (renderEvent rooms lecturers) (List.sortWith eventTupleComparator events) ++ List.map renderHiddenEvents (List.sortWith eventTupleComparator hiddenEvents))


{-| Transforms an event into a list item
-}
renderEvent : Dict RoomID Room -> Dict LecturerID Lecturer -> ( EventID, Event ) -> Html Msg
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
                            Lecturer "----" "----" ""

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" ""
    in
    li [ class "list-item", onClick (ItemClick (OnEventClick ( eventID, event ))) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%", attribute "title" event.subjectAbbr ] [ text event.subjectAbbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "35%", attribute "title" event.subject, style "margin-left" "1%" ] [ text event.subject ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "5%", style "margin-left" "1%" ] [ text (convertWeekDay event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%", style "margin-left" "1%" ] [ text (convertWeekTimeHourAndMinute event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%", style "margin-left" "1%" ] [ text (convertWeekTimeHourAndMinute event.end_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "15%", attribute "title" room.abbr, style "margin-left" "1%" ] [ text room.abbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "5%", style "margin-left" "1%" ] [ text (String.fromInt room.capacity) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%", style "margin-left" "1%" ] [ text lecturer.abbr ]
        ]


renderHiddenEvents : ( EventID, Event ) -> Html Msg
renderHiddenEvents ( eventID, event ) =
    li [ class "list-item", style "text-decoration" "line-through", onClick (EditMenu (EditEvent eventID)) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%", attribute "title" event.subjectAbbr ] [ text event.subjectAbbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "35%", attribute "title" event.subject, style "margin-left" "1%" ] [ text event.subject ]
        ]


{-| TODO: ADD ⚠️ emote to rooms with conflicts.
-}
renderRooms : Dict RoomID Room -> Dict RoomID Room -> Maybe ( RoomID, Room ) -> Html Msg
renderRooms rooms hiddenRooms selectedRoom =
    let
        roomsList =
            Dict.toList rooms |> List.sortWith roomTupleComparator

        hiddenRoomsList =
            Dict.toList hiddenRooms |> List.sortWith roomTupleComparator

        modifyIcon =
            case selectedRoom of
                Just ( id, _ ) ->
                    div [ class "gg-pen", onClick (EditMenu (EditRoom id)) ] []

                Nothing ->
                    div [ style "display" "none" ] []
    in
    ul [ class "list custom-scrollbar" ]
        (ul [ ariaLabel "Salas", class "list-title" ] [ modifyIcon, div [ class "gg-add", onClick (EditMenu AddRoom) ] [] ]
            :: List.map renderRoom roomsList
            ++ List.map renderHiddenRooms hiddenRoomsList
        )


renderRoom : ( RoomID, Room ) -> Html Msg
renderRoom ( int, room ) =
    li [ class "list-item", onClick (ItemClick (OnRoomClick ( int, room ))), attribute "title" room.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


renderHiddenRooms : ( RoomID, Room ) -> Html Msg
renderHiddenRooms ( int, room ) =
    li [ class "list-item", style "text-decoration" "line-through", attribute "title" room.name, onClick (EditMenu (EditRoom int)) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


{-| TODO: ADD ⚠️ emote to lecturers with conflicts
-}
renderLecturers : Dict LecturerID Lecturer -> Dict LecturerID Lecturer -> Maybe ( LecturerID, Lecturer ) -> Html Msg
renderLecturers lecturers hiddenLecturers selectedLect =
    let
        lecturersList =
            Dict.toList lecturers |> List.sortWith lectTupleComparator

        hiddenLecturersList =
            Dict.toList hiddenLecturers |> List.sortWith lectTupleComparator

        modifyIcon =
            case selectedLect of
                Just ( id, _ ) ->
                    div [ class "gg-pen", onClick (EditMenu (EditLect id)) ] []

                Nothing ->
                    div [ style "display" "none" ] []
    in
    ul [ class "list custom-scrollbar" ]
        (ul [ ariaLabel "Docentes", class "list-title" ] [ modifyIcon, div [ class "gg-add", onClick (EditMenu AddLect) ] [] ] :: List.map renderLecturer lecturersList ++ List.map renderHiddenLecturer hiddenLecturersList)


renderLecturer : ( LecturerID, Lecturer ) -> Html Msg
renderLecturer ( int, lecturer ) =
    li [ class "list-item", onClick (ItemClick (OnLecturerClick ( int, lecturer ))), attribute "title" lecturer.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]


renderHiddenLecturer : ( LecturerID, Lecturer ) -> Html Msg
renderHiddenLecturer ( int, lecturer ) =
    li [ class "list-item", style "text-decoration" "line-through", attribute "title" lecturer.name, onClick (EditMenu (EditLect int)) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]


renderBlocks : Dict BlockID Block -> Dict BlockID Block -> Maybe ( BlockID, Block ) -> Html Msg
renderBlocks blocks hiddenBlocks selectedBlock =
    let
        blocksList =
            Dict.toList blocks |> List.sortWith blockTupleComparator

        hiddenBlocksList =
            Dict.toList hiddenBlocks |> List.sortWith blockTupleComparator

        modifyIcon =
            case selectedBlock of
                Just ( id, _ ) ->
                    div [ class "gg-pen", onClick (EditMenu (EditBlock id)) ] []

                Nothing ->
                    div [ style "display" "none" ] []
    in
    ul [ class "list custom-scrollbar" ]
        (ul [ ariaLabel "Blocos", class "list-title" ] [ modifyIcon, div [ class "gg-add", onClick (EditMenu AddBlock) ] [] ] :: List.map renderBlock blocksList ++ List.map renderHiddenBlock hiddenBlocksList)


renderBlock : ( BlockID, Block ) -> Html Msg
renderBlock ( id, block ) =
    li [ class "list-item", onClick (ItemClick (OnBlockClick ( id, block ))), attribute "title" block.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text block.nameAbbr ] ]


renderHiddenBlock : ( BlockID, Block ) -> Html Msg
renderHiddenBlock ( id, block ) =
    li [ class "list-item", style "text-decoration" "line-through", attribute "title" block.name, onClick (EditMenu (EditBlock id)) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text block.nameAbbr ] ]


{-| Given a certain event, a list of all rooms and a list of all events, it returns what rooms are available to host that event.
-}
renderAvailableRooms : Maybe ( EventID, Event ) -> Dict RoomID Room -> List Event -> List Occupation -> Html Msg
renderAvailableRooms maybe rooms events occupations =
    case maybe of
        Just ( eventId, event ) ->
            if isJust event.start_time && isJust event.end_time then
                renderAvailableRooms_ ( eventId, event ) rooms events occupations

            else
                ul [ ariaLabel "Salas Livres", class "list custom-scrollbar" ] []

        _ ->
            ul [ ariaLabel "Salas Livres", class "list custom-scrollbar" ] []


renderAvailableRooms_ : ( EventID, Event ) -> Dict RoomID Room -> List Event -> List Occupation -> Html Msg
renderAvailableRooms_ ( eventId, event ) rooms events occupations =
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
    ul [ ariaLabel ("Salas Livres para " ++ event.subjectAbbr), class "list custom-scrollbar" ]
        (List.map (renderAvailableRoom eventId) availableRooms)


renderAvailableRoom : EventID -> ( RoomID, Room ) -> Html Msg
renderAvailableRoom evId ( roomId, room ) =
    li [ class "list-item", onClick (ItemClick (ChangeEventRoomClick evId roomId)), attribute "title" room.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text (room.abbr ++ "\t(" ++ String.fromInt room.capacity ++ ")") ] ]
