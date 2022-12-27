module Main exposing (..)

import Browser
import DisplayEvents exposing (..)
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
-- Question: The algorithm I know for calculating time scheduling colisions is O(n)=n². Should I continue using the grid column size equal to the number of events to display or should I calculate max number the colisions and use that as the grid column?
-- Question: How to transform the loop into functional programming?


type Model
    = Model Data ScheduleFilter


{-| WARNING: Do not create any entry with ID=0
-}
init : ( Model, Cmd Msg )
init =
    ( Model
        { rooms = fromList [ Room "DCC Lab. 2" "FC6_157 (Lab2)" 20, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30, Room "CCC Lab. 6" "FC2_222 (Lab3)(LongName)" 30 ]
        , lecturers = fromList [ Lecturer "N'Golo Kanté" "NGK" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Apikalia" "Ak" [] [] [], Lecturer "Sofi" "Ae" [] [] [], Lecturer "Lianne" "Ac" [] [] [], Lecturer "Mayur" "Aa" [] [] [], Lecturer "Kristine" "Af" [] [] [], Lecturer "Straton" "Az" [] [] [], Lecturer "Svanhild" "Am" [] [] [], Lecturer "Ayla" "Aç" [] [] [], Lecturer "Mayem" "Ai" [] [] [], Lecturer "Minakshi" "BA" [] [] [], Lecturer "Isaiah" "CA" [] [] [] ]
        , events =
            fromList
                [ Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just (ID 1)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 1))
                , Event "asdasd (CC4011)_TP.1" "Alga-TP2" (Just (ID 1)) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) (Just (ID 2))
                , Event "asdasd (CC4011)_TP.1" "Alga-TP45" (Just (ID 1)) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) (Just (ID 2))
                , Event "asdasd (CC4011)_TP.1" "Alga-TP44" (Just (ID 1)) (Just (WeekTime Time.Mon 12 30)) (Just (WeekTime Time.Mon 13 30)) (Just (ID 2))
                , Event "Harooo" "Alga-TPX" (Just (ID 1)) (Just (WeekTime Time.Mon 11 0)) (Just (WeekTime Time.Mon 14 0)) (Just (ID 2))
                , Event "Harooo" "Alga-TPY" (Just (ID 1)) (Just (WeekTime Time.Mon 15 0)) (Just (WeekTime Time.Mon 17 0)) (Just (ID 2))
                , Event "subject" "subjAbrr" (Just (ID 2)) (Just (WeekTime Time.Mon 11 30)) (Just (WeekTime Time.Mon 12 30)) (Just (ID 2))
                , Event "noRoomEvent" "noRoomEvent" Nothing (Just (WeekTime Time.Tue 11 30)) (Just (WeekTime Time.Tue 12 0)) (Just (ID 2))
                , Event "noLectEvent" "noLectEvent" (Just (ID 2)) (Just (WeekTime Time.Sat 11 30)) (Just (WeekTime Time.Sat 12 0)) Nothing
                , Event "noRoom&LecEvent" "noRoom&LecEvent" Nothing (Just (WeekTime Time.Wed 11 30)) (Just (WeekTime Time.Wed 12 0)) Nothing
                ]
        , blocks = []
        }
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) "" "" "")
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ItemClick OnItemClick


type OnItemClick
    = OnRoomClick Int
    | OnLecturerClick Int
    | OnEventClick Int


{-| Update Room / Lecturer parameters based on the msg received.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data (ScheduleFilter roomFilter lectFilter blockFilter roomName lectName blockName)) =
    let
        {- We Create a function to fetch a new Room/Lecturer filter and abbreviation based on a Room/Lecturer ID.
           If the Room/Lecturer to update is already the one being displayed then dont perform any action.
        -}
        createNewRoomFilter : Int -> Int -> Event -> Bool
        createNewRoomFilter roomid _ event =
            case event.room of
                Just (ID int) ->
                    int == roomid

                Nothing ->
                    False

        getRoomAbbr roomid =
            case Table.get (ID roomid) data.rooms of
                Just r ->
                    r.abbr

                Nothing ->
                    roomName

        createNewLectFilter : Int -> Int -> Event -> Bool
        createNewLectFilter lectid _ event =
            case event.lecturer of
                Just (ID int) ->
                    int == lectid

                Nothing ->
                    False

        getLectAbbr lectid =
            case Table.get (ID lectid) data.lecturers of
                Just r ->
                    r.abbr

                Nothing ->
                    lectName
    in
    case msg of
        -- Get all events with a certain Room ID and with it update the Room Filter and Abbr.
        ItemClick (OnRoomClick id) ->
            ( Model data (ScheduleFilter (createNewRoomFilter id) lectFilter blockFilter (getRoomAbbr id) lectName blockName), Cmd.none )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        ItemClick (OnLecturerClick id) ->
            ( Model data (ScheduleFilter roomFilter (createNewLectFilter id) blockFilter roomName (getLectAbbr id) blockName), Cmd.none )

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

                -- Updating Room Filter / Abbr
                ( updatedRoomFilter, updatedRoomName ) =
                    case eventGet of
                        Just event ->
                            case event.room of
                                Just (ID roomid) ->
                                    ( createNewRoomFilter roomid, getRoomAbbr roomid )

                                Nothing ->
                                    ( roomFilter, roomName )

                        _ ->
                            ( roomFilter, roomName )

                -- Updating Lecturer Filter / Abbr
                ( updatedLectFilter, updatedLectName ) =
                    case eventGet of
                        Just event ->
                            case event.lecturer of
                                Just (ID lectid) ->
                                    ( createNewLectFilter lectid, getLectAbbr lectid )

                                Nothing ->
                                    ( lectFilter, lectName )

                        _ ->
                            ( lectFilter, lectName )
            in
            ( Model data (ScheduleFilter updatedRoomFilter updatedLectFilter blockFilter updatedRoomName updatedLectName blockName), Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view (Model data (ScheduleFilter roomFilter lectFilter blockFilter roomName lectName blockName)) =
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
            [ ul [ ariaLabel "Blocos", class "list custom-scrollbar" ] []
            , renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (toList data.events) data.rooms data.lecturers
            ]
        , div [ class "grids-container" ] [ renderScheduleAbbr blockList ("Bloco:" ++ blockName), renderScheduleAbbr roomList ("Sala:" ++ roomName), renderScheduleAbbr lectList ("Docente:" ++ lectName) ]
        ]


renderSchedule : Int -> List ( Int, Event ) -> String -> Html Msg
renderSchedule tableWidth events title =
    let
        nothing22 =
            Debug.log "--------------" title

        widthStr =
            String.append (tableWidth |> String.fromInt) "%"

        -- Function to create Columns headers li.
        weekdayToHtml weekDay =
            li [ class ("day " ++ (weekDay |> toCssClassWeekDay)) ] [ weekDay |> toPortugueseWeekday |> text ]

        -- Creating Row Header e.g: List ["08:00", "08:30", .. , "19:00", "19:30"].
        timeblocksText =
            let
                -- List [8,8,9,9,10,10,..,18,18,19,19]
                hours =
                    List.sort (List.range 8 19 ++ List.range 8 19)

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
            List.map2 convertHourAndMinute hours minutes

        timeblocks =
            List.map2 (\index str -> li [ class ("time t" ++ String.fromInt index) ] [ text str ]) (List.range 0 23) timeblocksText

        -- Display Events Cells to be renders
        liDisplayEvents =
            let
                -- Events separated based on their Time.Weekday.
                evSortedByDays =
                    List.map (sortByWeekday events) displayedWeekDays

                -- Display Events separated based on their Time.Weekday.
                dEvSortedByDays =
                    List.map createDisplayEvents evSortedByDays

                -- Function to render all display events of a certain day
                renderDayDisplayEvents : ( List DisplayEvent, Int ) -> List (Html Msg)
                renderDayDisplayEvents ( dEvents, colLength ) =
                    List.map (renderDisplayEvent colLength) dEvents
            in
            List.foldl (++) [] (List.map renderDayDisplayEvents dEvSortedByDays)
    in
    div [ style "width" widthStr ]
        [ h3 [ style "margin" "unset" ] [ text title ]
        , ul [ class "calendar weekly-byhour", style "width" widthStr ]
            (List.map weekdayToHtml displayedWeekDays ++ timeblocks ++ List.repeat (24 * 5) (li [] []) ++ liDisplayEvents)
        ]


{-| Turns a Display Event into a HTML <li> tag.
ColLength corresponds to the maximum number of colision between events in a day.
TODO: Add Drag/Drop event.
-}
renderDisplayEvent : Int -> DisplayEvent -> Html msg
renderDisplayEvent colLength (DisplayEvent id ev dInfo) =
    let
        width =
            ((dInfo.colEnd + 1) - dInfo.colStart) * 100 // colLength

        leftMargin =
            (dInfo.colStart * 100) // colLength

        weekday =
            toCssClassWeekDay dInfo.day

        {- Feature to Improve Visibility. Makes event visibility priority go from left to right -}
        zIndex =
            String.fromInt (999 - dInfo.colStart)

        -- ++ ";grid-row:  t" ++ String.fromInt dInfo.lineStart ++ "   /  h" ++ String.fromInt dInfo.lineEnd
    in
    if List.member dInfo.day displayedWeekDays then
        li [ class "event work", style "style" ("grid-column: " ++ weekday), style "margin-left" (String.fromInt leftMargin ++ "%"), style "grid-row" ("t" ++ String.fromInt dInfo.lineStart ++ "   /  t" ++ String.fromInt dInfo.lineEnd), style "width" (String.fromInt width ++ "%"), style "grid-column" weekday, style "z-index" zIndex ] [ text ev.subjectAbbr ]

    else
        li [ style "display" "none" ] [ text ev.subjectAbbr ]


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
    li [ class "list-item", onClick (ItemClick (OnRoomClick int)), attribute "data-tooltip" room.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text room.abbr ] ]


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
    li [ class "list-item", onClick (ItemClick (OnLecturerClick int)), attribute "data-tooltip" lecturer.name ] [ div [ class "custom-scrollbar", class "list-text" ] [ text lecturer.abbr ] ]



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
