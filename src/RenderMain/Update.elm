module RenderMain.Update exposing (..)

import Dict
import DnD
import RenderMain.DisplayEvents exposing (..)
import RenderMain.Model exposing (Model(..))
import RenderMain.Msg exposing (..)
import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time
import Effect exposing (Effect)


{-| Update Block / Event / Room / Lecturer filters based on the msg received.
-}
update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model data filters draggable) =
    case msg of
        ItemClick clickMsg ->
            updateOnItemClick clickMsg (Model data filters draggable)

        DnDMsg dndmsg ->
            ( Model data filters (DnD.update dndmsg draggable), Effect.none )

        OnDrop ( _, weekTime ) eventID ->
            let
                event =
                    Dict.get eventID data.events
            in
            case event of
                Just ev ->
                    let
                        startingTime =
                            Maybe.withDefault (WeekTime Time.Mon endingHour endingMinute) ev.start_time

                        endingTime =
                            Maybe.withDefault (WeekTime Time.Mon endingHour endingMinute) ev.end_time

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
                    ( Model { data | events = newEvents } filters draggable, Effect.none )

                Nothing ->
                    ( Model data filters draggable, Effect.none )


updateOnItemClick : OnItemClick -> Model -> ( Model, Effect Msg )
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
            ( Model data { filters | block = \_ -> block.cond, blockName = block.nameAbbr } draggable, Effect.none )

        -- Get all events with a certain Room ID and with it update the Room Filter and Abbr.
        OnRoomClick id ->
            ( Model data { filters | room = createNewRoomFilter id, roomName = getRoomAbbr id } draggable, Effect.none )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        OnLecturerClick id ->
            ( Model data { filters | lect = createNewLectFilter id, lectName = getLectAbbr id } draggable, Effect.none )

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
            ( Model data { filters | room = updatedRoomFilter, lect = updatedLectFilter, roomName = updatedRoomName, lectName = updatedLectName } draggable, Effect.none )
