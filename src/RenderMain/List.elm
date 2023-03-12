module RenderMain.List exposing (renderBlocks, renderEvents, renderLecturers, renderRooms)
{-| Responsible for displaying a list of a certain resource (e.g. list of rooms).
-}
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.WeekTimeConverters exposing (..)
import RenderMain.Msg exposing (Msg(..), OnItemClick(..))


{-| Renders all the events into a list
-}
renderEvents : List ( Int, Event ) -> Dict RoomID Room -> Dict LecturerID Lecturer -> Html Msg
renderEvents events rooms lecturers =
    ul [ ariaLabel "Cadeiras", class "list custom-scrollbar" ]
        (List.map (renderEvent rooms lecturers) events)


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
            Dict.toList rooms
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
            Dict.toList lecturers
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
            Dict.toList blocks
    in
    ul [ ariaLabel "Blocos", class "list custom-scrollbar" ] (List.map renderBlock blocksList)


renderBlock : ( Int, Block ) -> Html Msg
renderBlock ( id, block ) =
    li [ class "list-item", onClick (ItemClick (OnBlockClick ( id, block ))), attribute "title" block.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text block.nameAbbr ] ]
