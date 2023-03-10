module Main exposing (..)

import Browser
import Dict exposing (Dict)
import DisplayEvents exposing (..)
import DnD
import DragDrop exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import ScheduleObjects exposing (..)
import Time exposing (..)



-- INFO: Hash function = (hour-8)*2+V(minute), V(minute) = 1 if minute >= 30, otherwise minute = 0. type alias Hashmap = Array (List Event).


type Model
    = Model Data ScheduleFilter Draggable


{-| Init Drag and Drop messages
-}
dnd : DnD.DraggableInit ( DropEvent, WeekTime ) ID Msg
dnd =
    DnD.init DnDMsg OnDrop


{-| WARNING: Do not create any entry with ID=0
-}
init : ( Model, Cmd Msg )
init =
    ( Model
        { rooms =
            Dict.fromList
                [ ( 1, Room "DCC Lab. 2" "FC6_157 (Lab2)" 20 "1.58" )
                , ( 2, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30 "1.93" )
                , ( 3, Room "CCC Lab. 6" "FC2_222 (Lab3)(LongName)" 30 "1.67" )
                ]
        , lecturers =
            Dict.fromList
                [ ( 1, Lecturer "N'Golo Kanté" "NGK" [] [] [] )
                , ( 2, Lecturer "Alberto" "Al" [] [] [] )
                , ( 3, Lecturer "Apikalia" "Ak" [] [] [] )
                , ( 4, Lecturer "Sofi" "Ae" [] [] [] )
                , ( 5, Lecturer "Lianne" "Ac" [] [] [] )
                , ( 6, Lecturer "Mayur" "Aa" [] [] [] )
                , ( 7, Lecturer "Kristine" "Af" [] [] [] )
                , ( 8, Lecturer "Straton" "Az" [] [] [] )
                , ( 9, Lecturer "Svanhild" "Am" [] [] [] )
                , ( 10, Lecturer "Ayla" "Aç" [] [] [] )
                , ( 11, Lecturer "Mayem" "Ai" [] [] [] )
                , ( 12, Lecturer "Minakshi" "BA" [] [] [] )
                , ( 13, Lecturer "Isaiah" "CA" [] [] [] )
                ]
        , events =
            Dict.fromList
                [ ( 1, Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just 1) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just 1) )
                , ( 2, Event "asdasd (CC4011)_TP.1" "Alga-TP2" (Just 1) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) (Just 2) )
                , ( 3, Event "asdasd (CC4011)_TP.1" "Alga-TP45" (Just 1) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) (Just 2) )
                , ( 4, Event "asdasd (CC4011)_TP.1" "Alga-TP44" (Just 1) (Just (WeekTime Time.Mon 12 30)) (Just (WeekTime Time.Mon 13 30)) (Just 2) )
                , ( 5, Event "Harooo" "Alga-TPX" (Just 1) (Just (WeekTime Time.Mon 11 0)) (Just (WeekTime Time.Mon 14 0)) (Just 2) )
                , ( 6, Event "Harooo" "Alga-TPY" (Just 1) (Just (WeekTime Time.Mon 15 0)) (Just (WeekTime Time.Mon 17 0)) (Just 2) )
                , ( 7, Event "subject" "subjAbrr" (Just 2) (Just (WeekTime Time.Mon 11 30)) (Just (WeekTime Time.Mon 12 30)) (Just 2) )
                , ( 8, Event "noRoomEvent" "noRoomEvent" Nothing (Just (WeekTime Time.Tue 11 30)) (Just (WeekTime Time.Tue 12 0)) (Just 2) )
                , ( 9, Event "noLectEvent" "noLectEvent" (Just 2) (Just (WeekTime Time.Sat 11 30)) (Just (WeekTime Time.Sat 12 0)) Nothing )
                , ( 10, Event "noRoom&LecEvent" "noRoom&LecEvent" Nothing (Just (WeekTime Time.Wed 9 30)) (Just (WeekTime Time.Wed 12 0)) Nothing )
                ]
        , blocks =
            Dict.fromList
                [ ( 1, Block "All Events" "All Events" (\_ -> True) )
                , ( 2, Block "Eventos de Alga" "(CC4011)" (\ev -> String.contains "(CC4011)" ev.subject) )
                ]
        }
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) "" "" "")
        dnd.model
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ItemClick OnItemClick
    | DnDMsg (DnD.Msg ( DropEvent, WeekTime ) ID)
    | OnDrop ( DropEvent, WeekTime ) ID


type OnItemClick
    = OnRoomClick Int
    | OnLecturerClick Int
    | OnEventClick Int
    | OnBlockClick ( Int, Block )


{-| Update Room / Lecturer parameters based on the msg received.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data filters draggable) =
    case msg of
        ItemClick clickMsg ->
            updateOnItemClick clickMsg (Model data filters draggable)

        DnDMsg dndmsg ->
            ( Model data filters (DnD.update dndmsg draggable), Cmd.none )

        OnDrop ( _, weekTime ) eventID ->
            let
                event =
                    Dict.get eventID data.events
            in
            case event of
                Just ev ->
                    let
                        startingTime =
                            Maybe.withDefault (WeekTime Time.Mon endingHour DisplayEvents.endingMinute) ev.start_time

                        endingTime =
                            Maybe.withDefault (WeekTime Time.Mon endingHour DisplayEvents.endingMinute) ev.end_time

                        isTopSwitch =
                            if startingTime.minute == 30 then
                                -1

                            else
                                0

                        isBotSwitch =
                            if endingTime.minute == 30 then
                                1

                            else
                                0

                        slotsOccupied =
                            (endingTime.hour - startingTime.hour) * 2 + isTopSwitch + isBotSwitch

                        newEndTime =
                            --| Target slot is of the format "xx:30" and the event occupies an EVEN number of slots.
                            if weekTime.minute == 30 && (modBy 2 slotsOccupied == 0) then
                                WeekTime startingTime.weekday (weekTime.hour + (slotsOccupied // 2)) 30

                            else if weekTime.minute == 30 && (modBy 2 slotsOccupied == 1) then
                                -- Target slot is of the format "xx:30" and the event occupies an ODD number of slots.
                                WeekTime startingTime.weekday (weekTime.hour + (slotsOccupied // 2) + 1) 0

                            else if weekTime.minute == 0 && (modBy 2 slotsOccupied == 0) then
                                -- Target slot is of the format "xx:00" and the event occupies an EVEN number of slots.
                                WeekTime startingTime.weekday (weekTime.hour + (slotsOccupied // 2)) 0

                            else
                                -- Target slot is of the format "xx:00" and the event occupies an ODD number of slots.
                                WeekTime startingTime.weekday (weekTime.hour + (slotsOccupied // 2)) 30

                        newEv =
                            -- { ev | start_time = Just weekTime }
                            { ev | start_time = Just weekTime, end_time = Just newEndTime }

                        newEvents =
                            Dict.insert eventID newEv data.events
                    in
                    ( Model { data | events = newEvents } filters draggable, Cmd.none )

                Nothing ->
                    ( Model data filters draggable, Cmd.none )


updateOnItemClick : OnItemClick -> Model -> ( Model, Cmd Msg )
updateOnItemClick msg (Model data filters draggable) =
    let
        {- We Create a function to fetch a new Room/Lecturer filter and abbreviation based on a Room/Lecturer ID.
           If the Room/Lecturer to update is already the one being displayed then dont perform any action.
        -}
        createNewRoomFilter : Int -> Int -> Event -> Bool
        createNewRoomFilter roomid _ event =
            case event.room of
                Just id ->
                    id == roomid

                Nothing ->
                    False

        getRoomAbbr roomid =
            case Dict.get roomid data.rooms of
                Just r ->
                    r.abbr

                Nothing ->
                    filters.roomName

        createNewLectFilter : Int -> Int -> Event -> Bool
        createNewLectFilter lectid _ event =
            case event.lecturer of
                Just id ->
                    id == lectid

                Nothing ->
                    False

        getLectAbbr lectid =
            case Dict.get lectid data.lecturers of
                Just r ->
                    r.abbr

                Nothing ->
                    filters.lectName
    in
    case msg of
        OnBlockClick ( _, block ) ->
            ( Model data { filters | block = \_ -> block.cond, blockName = block.nameAbbr } draggable, Cmd.none )

        -- Get all events with a certain Room ID and with it update the Room Filter and Abbr.
        OnRoomClick id ->
            ( Model data { filters | room = createNewRoomFilter id, roomName = getRoomAbbr id } draggable, Cmd.none )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        OnLecturerClick id ->
            ( Model data { filters | lect = createNewLectFilter id, lectName = getLectAbbr id } draggable, Cmd.none )

        {-
           For an Event click we need to change both the room Filter and the Lecturer Filter.
           Get all events with a certain Room ID and with it update the Room Filter.
           Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
           If the event ID received is not valid then no changes will occur.
        -}
        OnEventClick id ->
            let
                -- Getting the event from the Dict of Events
                eventGet =
                    Dict.get id data.events

                -- Updating Room Filter / Abbr
                ( updatedRoomFilter, updatedRoomName ) =
                    case eventGet of
                        Just event ->
                            case event.room of
                                Just roomid ->
                                    ( createNewRoomFilter roomid, getRoomAbbr roomid )

                                Nothing ->
                                    ( filters.room, filters.roomName )

                        _ ->
                            ( filters.room, filters.roomName )

                -- Updating Lecturer Filter / Abbr
                ( updatedLectFilter, updatedLectName ) =
                    case eventGet of
                        Just event ->
                            case event.lecturer of
                                Just lectid ->
                                    ( createNewLectFilter lectid, getLectAbbr lectid )

                                Nothing ->
                                    ( filters.lect, filters.lectName )

                        _ ->
                            ( filters.lect, filters.lectName )
            in
            ( Model data { filters | room = updatedRoomFilter, lect = updatedLectFilter, roomName = updatedRoomName, lectName = updatedLectName } draggable, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view (Model data filters draggable) =
    let
        tableWidth =
            90 / (3 |> toFloat) |> floor

        -- To shorten the function call
        renderScheduleAbbr =
            renderSchedule tableWidth draggable

        -- Here we render the filters, turning them from (Int -> Event -> Bool) into a List (Int, Event)
        roomList =
            Dict.filter filters.room data.events |> Dict.toList

        lectList =
            Dict.filter filters.lect data.events |> Dict.toList

        blockList =
            Dict.filter filters.block data.events |> Dict.toList

        dragged : ID -> Html Msg
        dragged id =
            div [] [ id |> Debug.toString |> text ]
    in
    div []
        [ div [ class "listbox-area" ]
            [ renderBlocks data.blocks
            , renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (Dict.toList data.events) data.rooms data.lecturers
            ]
        , div [ class "grids-container" ] [ renderScheduleAbbr blockList ("Bloco:" ++ filters.blockName), renderScheduleAbbr roomList ("Sala:" ++ filters.roomName), renderScheduleAbbr lectList ("Docente:" ++ filters.lectName) ]
        , DnD.dragged
            draggable
            dragged
        ]


renderSchedule : Int -> Draggable -> List ( Int, Event ) -> String -> Html Msg
renderSchedule tableWidth draggable events title =
    let
        -- nothing22 =
        --     Debug.log "--------------" title
        -- nothing33 =
        --     Debug.log "List of (ID,Event)" events
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

        -- Empty slots (with no events). They possess a event listener that triggers on the case of an event that is dragged to it (Triggers a Msg onDrop).
        liWeekSlots =
            let
                -- List [8,8,9,9,10,10,..,18,18,19,19]
                hours =
                    List.sort (List.range startingHour endingHour ++ List.range startingHour endingHour)

                getMinute index =
                    if modBy 2 index == 0 then
                        0

                    else
                        30

                -- Create empty slots for a line of a schedule
                lineWeekLi =
                    \index hour -> List.map (\weekday -> li [] [ dnd.droppable ( RoomEvent 1, WeekTime weekday hour (getMinute index) ) [ style "height" "100%", style "width" "100%" ] [] ]) displayedWeekDays

                -- lineWeekLi = (\index hour -> List.map (\weekday -> li [] [ dnd.droppable ( RoomEvent 1, WeekTime weekday hour (getMinute index) ) [ style "height" "100%", style "width" "100%" ] [  Debug.toString weekday ++ " " ++ Debug.toString hour ++ ":" ++ Debug.toString (getMinute index) |> text ] ]) displayedWeekDays)
            in
            List.indexedMap lineWeekLi hours |> List.concat

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
                    List.map (renderDisplayEvent colLength draggable) dEvents
            in
            List.foldl (++) [] (List.map renderDayDisplayEvents dEvSortedByDays)
    in
    div [ style "width" widthStr ]
        [ h3 [ style "margin" "unset" ] [ text title ]
        , ul [ class "calendar weekly-byhour", style "width" widthStr ]
            -- (List.map weekdayToHtml displayedWeekDays ++ timeblocks ++ List.repeat (24 * 5) (li [] []) ++ liDisplayEvents)
            (List.map weekdayToHtml displayedWeekDays ++ timeblocks ++ liWeekSlots ++ liDisplayEvents)
        ]


{-| Turns a Display Event into a HTML <li> tag.
ColLength corresponds to the maximum number of colision between events in a day.
-}
renderDisplayEvent : Int -> Draggable -> DisplayEvent -> Html Msg
renderDisplayEvent colLength draggable (DisplayEvent id ev dInfo) =
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

        {- Hide DisplayEvents when we want to drop a displayEvent -}
        hideAtt =
            case DnD.getDragMeta draggable of
                Just _ ->
                    [ style "pointer-events" "none" ]

                _ ->
                    []
    in
    if List.member dInfo.day displayedWeekDays then
        li ([ class "event work", style "style" ("grid-column: " ++ weekday), style "margin-left" (String.fromInt leftMargin ++ "%"), style "grid-row" ("t" ++ String.fromInt dInfo.lineStart ++ "   /  t" ++ String.fromInt dInfo.lineEnd), style "width" (String.fromInt width ++ "%"), style "grid-column" weekday, style "z-index" zIndex, attribute "title" ev.subjectAbbr ] ++ hideAtt) [ dnd.draggable id [ style "height" "-webkit-fill-available", style "width" "-webkit-fill-available" ] [ text ev.subjectAbbr ] ]

    else
        li [ style "display" "none" ] [ text ev.subjectAbbr ]


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
                            Lecturer "----" "----" [] [] []

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" [] [] []
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


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ draggable) =
    dnd.subscriptions draggable



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
