module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
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


filterIndex : { lecturer : Int, room : Int }
filterIndex =
    { lecturer = 1
    , room = 2
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        { rooms = fromList [ Room "DCC Lab. 2" "FC6_157 (Lab2)" 20, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30, Room "CCC Lab. 6" "FC2_222 (Lab3)(LongName)" 30 ]
        , lecturers = fromList [ Lecturer "N'Golo Kanté" "NGK" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [] ]
        , events =
            fromList
                [ Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just (ID 0)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 0))
                , Event "asdasd (CC4011)_TP.1" "Alga-TP2" (Just (ID 0)) (Just (WeekTime Time.Mon 11 30)) (Just (WeekTime Time.Mon 12 0)) (Just (ID 1))
                , Event "subject" "subjAbrr" (Just (ID 1)) (Just (WeekTime Time.Mon 11 30)) (Just (WeekTime Time.Mon 12 0)) (Just (ID 1))
                ]
        , blocks = []
        }
        (Array.fromList
            [ []
            , []
            , []
            ]
        )
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
        -- Get all events with a certain room ID, construct a ScheduleFilter and update it in the Model
        ClickedRoom id ->
            let
                roomFilter : Int -> Event -> Bool
                roomFilter _ event =
                    case event.room of
                        Just (ID int) ->
                            int == id

                        Nothing ->
                            False

                filteredEvents : ScheduleFilter
                filteredEvents =
                    filter roomFilter data.events
            in
            ( Model data (Array.set filterIndex.room filteredEvents filters), Cmd.none )

        _ ->
            ( Model data filters, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view (Model data filters) =
    let
        tableWidth =
            90 / (Array.length filters |> toFloat) |> floor

        -- x = Debug.log "" tableWidth
    in
    div []
        [ div [ class "listbox-area" ]
            [ renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (toList data.events) data.rooms data.lecturers
            ]
        , div [ class "table-container" ] (Array.map (renderSchedule tableWidth) filters |> Array.toList)
        ]


renderSchedule : Int -> ScheduleFilter -> Html Msg
renderSchedule tableWidth schedule =
    let
        widthStr =
            String.append (tableWidth |> String.fromInt) "%"

    in
    table [ class "calender", class "table", class "table-bordered", style "width" widthStr ] []


{-| Renders all the events into a list
-}
renderEvents : List ( Int, Event ) -> Table Room -> Table Lecturer -> Html Msg
renderEvents events rooms lecturers =
    ul [ ariaLabel "Cadeiras", class "list custom-scrollbar" ]
        (List.map (renderEvent rooms lecturers) events)


{-| Transforms an event into a list item
-}
renderEvent : Table Room -> Table Lecturer -> ( Int, Event ) -> Html Msg
renderEvent rooms lecturers ( eventID, event ) =
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
    li [ class "list-item", onClick (ClickedEvent eventID) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text event.subjectAbbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "35%" ] [ text event.subject ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "5%" ] [ text (convertWeekDay event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertHourAndMinute event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertHourAndMinute event.end_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text room.abbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (String.fromInt room.capacity) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text lecturer.abbr ]
        ]


renderRooms : Table Room -> Html Msg
renderRooms rooms =
    let
        roomsList =
            Table.toList rooms
    in
    ul [ ariaLabel "Salas", class "list custom-scrollbar" ]
        (List.map renderRoom roomsList)


renderRoom : ( Int, Room ) -> Html Msg
renderRoom ( int, room ) =
    li [ class "list-item", onClick (ClickedRoom int) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


renderLecturers : Table Lecturer -> Html Msg
renderLecturers lecturers =
    let
        lecturersList =
            Table.toList lecturers
    in
    ul [ ariaLabel "Docentes", class "list custom-scrollbar" ]
        (List.map renderLecturer lecturersList)


renderLecturer : ( Int, Lecturer ) -> Html Msg
renderLecturer ( int, lecturer ) =
    li [ class "list-item", onClick (ClickedLecturer int) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]


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
