module Main exposing (..)

import Array exposing (Array)
import Browser
import DisplayEvents exposing (..)
import Exts.Html exposing (nbsp)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import ScheduleObjects exposing (..)
import Table exposing (..)
import Time exposing (..)



-- TODO: Remove data in init and read from a json file
-- TODO: Represent blocks by function that receives and event and outputs a bool
-- INFO: Hash function = (hour-8)*2+V(minute), V(minute) = 1 if minute >= 30, otherwise minute = 0. type alias Hashmap = Array (List Event).
-- DUVIDA: padding in .calendar li{}

type Model
    = Model Data ScheduleFilter


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
                , Event "noRoomEvent" "noRoomEvent" Nothing (Just (WeekTime Time.Tue 11 30)) (Just (WeekTime Time.Tue 12 0)) (Just (ID 1))
                , Event "noLectEvent" "noLectEvent" (Just (ID 1)) (Just (WeekTime Time.Sat 11 30)) (Just (WeekTime Time.Sat 12 0)) Nothing
                , Event "noRoom&LecEvent" "noRoom&LecEvent" Nothing (Just (WeekTime Time.Wed 11 30)) (Just (WeekTime Time.Wed 12 0)) Nothing
                ]
        , blocks = []
        }
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False))
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ItemClick OnItemClick


type OnItemClick
    = OnRoomClick Int
    | OnLecturerClick Int
    | OnEventClick Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data (ScheduleFilter roomFilter lectFilter blockFilter)) =
    case msg of
        -- Get all events with a certain Room ID and with it update the Room Filter.
        ItemClick (OnRoomClick id) ->
            let
                newRoomFilter : Int -> Event -> Bool
                newRoomFilter _ event =
                    case event.room of
                        Just (ID int) ->
                            int == id

                        Nothing ->
                            False
            in
            ( Model data (ScheduleFilter newRoomFilter lectFilter blockFilter), Cmd.none )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        ItemClick (OnLecturerClick id) ->
            let
                newLectFilter : Int -> Event -> Bool
                newLectFilter _ event =
                    case event.lecturer of
                        Just (ID int) ->
                            int == id

                        Nothing ->
                            False
            in
            ( Model data (ScheduleFilter roomFilter newLectFilter blockFilter), Cmd.none )

        {-
           For an Event click we need to change both the room Filter and the Lecturer Filter.
           Get all events with a certain Room ID and with it update the Room Filter.
           Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
           If the event ID received is not valid then no changes will occur.
        -}
        ItemClick (OnEventClick id) ->
            let
                -- Getting the event from the Table of Events
                eventGet =
                    Table.get (ID id) data.events

                {-
                   This double assignment searches if the event has a room and a lecturer. If the event has a room/lecturer exist then the filter is replaced with the matching room/lecturer filter, otherwise, we maintain the previous filter.
                -}
                -- This can be reduced! I simply do not know how, I suspect it's with the use of Maybe.AndThen().
                ( updatedRoomFilter, updatedLectFilter ) =
                    case eventGet of
                        Just event ->
                            let
                                newroomfilter =
                                    case event.room of
                                        Just (ID roomID) ->
                                            let
                                                newRoomFilter : Int -> Event -> Bool
                                                newRoomFilter _ ev =
                                                    case ev.room of
                                                        Just (ID int) ->
                                                            int == roomID

                                                        Nothing ->
                                                            False
                                            in
                                            newRoomFilter

                                        _ ->
                                            roomFilter

                                newLectfilter =
                                    case event.lecturer of
                                        Just (ID lectID) ->
                                            let
                                                newLectFilter : Int -> Event -> Bool
                                                newLectFilter _ ev =
                                                    case ev.lecturer of
                                                        Just (ID int) ->
                                                            int == lectID

                                                        Nothing ->
                                                            False
                                            in
                                            newLectFilter

                                        _ ->
                                            lectFilter
                            in
                            ( newroomfilter, newLectfilter )

                        _ ->
                            ( roomFilter, lectFilter )
            in
            ( Model data (ScheduleFilter updatedRoomFilter updatedLectFilter blockFilter), Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view (Model data (ScheduleFilter roomFilter lectFilter blockFilter)) =
    let
        tableWidth =
            90 / (3 |> toFloat) |> floor

        -- To shorten the function call
        renderScheduleAbbr =
            renderSchedule tableWidth

        -- Here we render the filters, turning them from (Int -> Event -> Bool) into a List (Int, Event)
        roomList =
            filter roomFilter data.events

        lectList =
            filter lectFilter data.events

        blockList =
            filter blockFilter data.events
    in
    div []
        [ div [ class "listbox-area" ]
            [ renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (toList data.events) data.rooms data.lecturers
            ]
        , div [ class "grids-container" ] [ renderScheduleAbbr roomList "Salas", renderScheduleAbbr lectList "Docentes", renderScheduleAbbr blockList "Blocos" ]
        ]


renderSchedule : Int -> List ( Int, Event ) -> String -> Html Msg
renderSchedule tableWidth filter title =
    let
        widthStr =
            String.append (tableWidth |> String.fromInt) "%"

        mon =
            li [ class "day mon" ] [ text "Seg" ]

        tue =
            li [ class "day tue" ] [ text "Terç" ]

        wed =
            li [ class "day wed" ] [ text "Qua" ]

        thu =
            li [ class "day thu" ] [ text "Qui" ]

        fri =
            li [ class "day fri" ] [ text "Sex" ]

        -- List ["08:00", "08:30", .. , "19:00", "19:30"]
        timeblocksText =
            let
                -- List [8,8,9,9,10,10,..,18,18,19,19]
                hours =
                    List.sort ((List.range 8 19) ++ (List.range 8 19))

                -- List [0,30,0,30,30, ...]
                minutes =
                    List.indexedMap
                        (\index _ ->
                            if modBy 2 index == 0 then
                                0

                            else
                                30
                        )
                        (List.repeat 24 0)
            in
            List.map2 convertHourAndMinute (Debug.log "hours" hours) (Debug.log "minutes" minutes)

        timeblocks =
            List.map2 (\index str -> li [ class ("time t" ++ String.fromInt index) ] [ text str ]) (List.range 0 23) (Debug.log "table" timeblocksText)
    in
    ul [ class "calendar weekly-byhour", style "width" widthStr ]
        ([ mon, tue, wed, thu, fri ] ++ timeblocks ++ List.repeat (24 * 5) (li [] []))



-- |> Debug.log "1:" timeblocks
-- Debug table
-- table [ class "calender", class "table", class "table-bordered", style "width" widthStr ] [ caption [] [ text <| title ], text <| Debug.toString <| filter ]


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
    li [ class "list-item", onClick (ItemClick (OnEventClick eventID)) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text event.subjectAbbr ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "35%" ] [ text event.subject ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "5%" ] [ text (convertWeekDay event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertWeekTimeHourAndMinute event.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text (convertWeekTimeHourAndMinute event.end_time) ]
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
    li [ class "list-item", onClick (ItemClick (OnRoomClick int)) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


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
    li [ class "list-item", onClick (ItemClick (OnLecturerClick int)) ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
