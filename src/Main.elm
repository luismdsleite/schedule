module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ScheduleObjects exposing (..)
import Table exposing (..)
import Time exposing (..)



-- TODO: Remove data in init and read from a json file
-- TODO: Represent blocks by function that receives and event and outputs a bool
---- MODEL ----
-- type alias Model =
--     { data : Data
--     , filters : Array ScheduleFilter
--     }


type Model
    = Model Data (Array ScheduleFilter)


type alias Data =
    { rooms : Table Room
    , lecturers : Table Lecturer
    , events : Table Event
    , blocks : List Block
    }



-- type ScheduleFilter
--     = RoomFilter (List ( Int, Event ))
--     | LecturerFilter (List ( Int, Event ))
--     | BlockFilter (List ( Int, Event ))


{-| Has the data required to represent a schedule
-}
type alias ScheduleFilter =
    List ( Int, Event )


{-| Array Indexes of where a certain ScheduleFilter is stored
-}
type alias FilterIndex =
    { room : Int
    , lecturer : Int
    , block : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        { rooms = fromList [ Room "DCC Lab. 2" "FC6_157 (Lab2)" 20, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30, Room "CCC Lab. 6" "FC6_177 (Lab3)(LongName)" 30 ]
        , lecturers = fromList [ Lecturer "N'Golo Kanté" "NGK" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [] ]
        , events =
            fromList
                [ Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just (ID 0)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 0))
                , Event "asdasd (CC4010)_TP.1" "Alga-TP2" (Just (ID 0)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 1))
                ]
        , blocks = []
        }
        (Array.repeat 3 [])
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ClickedRoom Int -- Selected a specific Room
    | ClickedLecturer Int -- Selected a specific Lecturer
    | ClickedEvent Int -- Selected a specific Event


-- Error: Compiler complains when I add Clicked to Msg
-- type Clicked =
--     -- | OnBlockClick BlockID -- Selected a specific Block
--     ClickedRoom Int -- Selected a specific Room
--     | ClickedLecturer Int -- Selected a specific Lecturer
--     | ClickedEvent Int -- Selected a specific Event

update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data filters) =
    case msg of
        -- ClickedLecturer id -> ( (Model {data | rooms = fromList[]} filters), Cmd.none )
        _ -> ( (Model data filters), Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view (Model data _) =
    div []
        [ div [ class "listbox-area" ]
            [ renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (toList data.events) data.rooms data.lecturers
            ]
        ]


renderEvents : List (Int,Event) -> Table Room -> Table Lecturer -> Html Msg
renderEvents events rooms lecturers =
    ul [ class "list custom-scrollbar" ]
        (List.map (renderEvent rooms lecturers) events)


{-| Transforms an event into a list item
-}
renderEvent : Table Room -> Table Lecturer -> (Int,Event) -> Html Msg
renderEvent rooms lecturers (eventID, event) =
    let
        room =
            case event.room of
                Just roomID ->
                    case Table.get roomID rooms of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Room "----" "----" -1

                -- Event still has no room assigned
                Nothing ->
                    Room "----" "----" -1

        lecturer =
            case event.lecturer of
                Just lecturerID ->
                    case Table.get lecturerID lecturers of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Lecturer "----" "----" [] [] []

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" [] [] []
    in
    li [ class "list-item", onClick (ClickedEvent eventID)]
        [ div [ style "width" "10%" ] [ text event.subjectAbbr ]
        , div [ style "width" "35%" ] [ text event.subject ]
        , div [ style "width" "5%" ] [ text (convertWeekDay event.start_time) ]
        , div [ style "width" "10%" ] [ text (convertHourAndMinute event.start_time) ]
        , div [ style "width" "10%" ] [ text (convertHourAndMinute event.end_time) ]
        , div [ style "width" "10%" ] [ text room.abbr ]
        , div [ style "width" "10%" ] [ text (String.fromInt room.capacity) ]
        , div [ style "width" "10%" ] [ text lecturer.abbr ]
        ]


renderRooms : Table Room -> Html Msg
renderRooms rooms =
    let
        roomsList =
            Table.toList rooms
    in
    ul [ class "list custom-scrollbar" ]
        (List.map renderRoom roomsList)


renderRoom : ( Int, Room ) -> Html Msg
renderRoom ( int, room ) =
    li [ class "list-item", onClick (ClickedRoom int)] [ div [] [ text room.abbr ] ]


renderLecturers : Table Lecturer -> Html Msg
renderLecturers lecturers =
    let
        lecturersList =
            Table.toList lecturers
    in
    ul [ class "list custom-scrollbar" ]
        (List.map renderLecturer lecturersList)


renderLecturer : ( Int, Lecturer ) -> Html Msg
renderLecturer ( int, lecturer ) =
    li [ class "list-item", onClick (ClickedLecturer int) ] [ div [] [ text lecturer.abbr ] ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- createRoomID : Int -> RoomID
-- createRoomID int = RoomID(int)
