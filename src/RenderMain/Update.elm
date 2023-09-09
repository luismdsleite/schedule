module RenderMain.Update exposing (..)

import Browser.Dom exposing (Error(..))
import Dict
import DnD
import Effect exposing (Effect)
import Encoders
import Http
import RenderMain.DisplayEvents exposing (..)
import RenderMain.Model exposing (Model(..))
import RenderMain.Msg exposing (..)
import Route.Path
import ScheduleObjects.Data exposing (Token)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time


{-| Update Block / Event / Room / Lecturer filters based on the msg received.
-}
update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model data filters draggable selectedItems) =
    case msg of
        ItemClick clickMsg ->
            updateOnItemClick clickMsg (Model data filters draggable selectedItems)

        DnDMsg dndmsg ->
            ( Model data filters (DnD.update dndmsg draggable) selectedItems, Effect.none )

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
                                WeekTime weekTime.weekday (weekTime.hour + (slotsOccupied // 2)) 30

                            else if weekTime.minute == 30 && (modBy 2 slotsOccupied == 1) then
                                -- Target slot is of the format "xx:30" and the event occupies an ODD number of slots.
                                WeekTime weekTime.weekday (weekTime.hour + (slotsOccupied // 2) + 1) 0

                            else if weekTime.minute == 0 && (modBy 2 slotsOccupied == 0) then
                                -- Target slot is of the format "xx:00" and the event occupies an EVEN number of slots.
                                WeekTime weekTime.weekday (weekTime.hour + (slotsOccupied // 2)) 0

                            else
                                -- Target slot is of the format "xx:00" and the event occupies an ODD number of slots.
                                WeekTime weekTime.weekday (weekTime.hour + (slotsOccupied // 2)) 30

                        newEv =
                            { ev | start_time = Just weekTime, end_time = Just newEndTime }
                    in
                    -- ( Model { data | events = newEvents } filters draggable, Effect.none )
                    ( Model data filters draggable selectedItems, Effect.sendCmd (updateEvent ( eventID, newEv ) data.backendUrl data.token) )

                Nothing ->
                    ( Model data filters draggable selectedItems, Effect.none )

        UpdateEvent result ->
            case result of
                Ok ( evID, ev ) ->
                    let
                        newEvents =
                            Dict.insert evID ev data.events

                        updatedSelectedItems =
                            -- If the timeslots for the event we are searching for available rooms are modified, we must update the 'selectedItems.event' variable.
                            case selectedItems.event of
                                Just ( selectedId, _ ) ->
                                    if selectedId == evID then
                                        { selectedItems | event = Just ( selectedId, ev ) }

                                    else
                                        selectedItems

                                Nothing ->
                                    selectedItems
                    in
                    ( Model { data | events = newEvents } filters draggable updatedSelectedItems, Effect.updateEvent ( evID, ev ) Nothing )

                Err _ ->
                    ( Model data filters draggable selectedItems, Effect.none )

        EditMenu menuMsg ->
            updateOnMenuEdit menuMsg (Model data filters draggable selectedItems)



-- UrlChange sharedData route ->
--     let
--         c =
--             Dict.get 1620 sharedData.events
--         a =
--             case c of
--                 Just ev ->
--                     Debug.log "ev" ev
--                 Nothing ->
--                     Debug.log "Nothing" (Event "" "" Nothing Nothing Nothing Nothing)
--     in
--     ( Model data filters draggable selectedItems, Effect.none )


updateOnItemClick : OnItemClick -> Model -> ( Model, Effect Msg )
updateOnItemClick msg (Model data filters draggable selectedItems) =
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
                    r.name

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
                    r.name

                Nothing ->
                    filters.lectName

        createNewOccFilter : ID -> OccupationID -> Occupation -> Bool
        createNewOccFilter roomId _ occ =
            roomId == occ.room

        createNewRestFilter : ID -> RestrictionID -> Restriction -> Bool
        createNewRestFilter lectId _ rest =
            lectId == rest.lect
    in
    case msg of
        OnBlockClick ( blockId, block ) ->
            ( Model data { filters | block = block.cond, blockName = block.name } draggable { selectedItems | block = Just ( blockId, block ) }, Effect.none )

        -- Get all events with a certain Room ID and with it update the Room Filter and Abbr.
        OnRoomClick ( id, room ) ->
            ( Model data { filters | room = createNewRoomFilter id, roomName = getRoomAbbr id, occupations = createNewOccFilter id } draggable { selectedItems | room = Just ( id, room ) }, Effect.none )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        OnLecturerClick ( id, lect ) ->
            ( Model data { filters | lect = createNewLectFilter id, lectName = getLectAbbr id, restrictions = createNewRestFilter id } draggable { selectedItems | lect = Just ( id, lect ) }, Effect.none )

        {-
           For an Event click we need to change both the room Filter and the Lecturer Filter.
           Get all events with a certain Room ID and with it update the Room Filter.
           Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
           If the event ID received is not valid then no changes will occur.
        -}
        OnEventClick ( evID, ev ) ->
            let
                -- Updating Room Filter / Abbr
                ( updatedRoomFilter, updatedRoomName, updatedOccupationsFilter ) =
                    case ev.room of
                        Just roomid ->
                            ( createNewRoomFilter roomid, getRoomAbbr roomid, createNewOccFilter roomid )

                        Nothing ->
                            ( filters.room, filters.roomName, filters.occupations )

                -- Updating Lecturer Filter / Abbr
                ( updatedLectFilter, updatedLectName, updatedRestrictionsFilter ) =
                    case ev.lecturer of
                        Just lectid ->
                            ( createNewLectFilter lectid, getLectAbbr lectid, createNewRestFilter lectid )

                        Nothing ->
                            ( filters.lect, filters.lectName, filters.restrictions )

                updatedSelectedRoom =
                    case ev.room of
                        Just roomId ->
                            case Dict.get roomId data.rooms of
                                Just room ->
                                    Just ( roomId, room )

                                Nothing ->
                                    selectedItems.room

                        Nothing ->
                            selectedItems.room

                updatedSelectedLect =
                    case ev.lecturer of
                        Just lectId ->
                            case Dict.get lectId data.lecturers of
                                Just lect ->
                                    Just ( lectId, lect )

                                Nothing ->
                                    selectedItems.lect

                        Nothing ->
                            selectedItems.lect

                updatedSelectedItems =
                    { selectedItems | event = Just ( evID, ev ), room = updatedSelectedRoom, lect = updatedSelectedLect }
            in
            ( Model data { filters | room = updatedRoomFilter, lect = updatedLectFilter, roomName = updatedRoomName, lectName = updatedLectName, occupations = updatedOccupationsFilter, restrictions = updatedRestrictionsFilter } draggable updatedSelectedItems, Effect.none )

        ChangeEventRoomClick evId roomId ->
            let
                eventGet =
                    Dict.get evId data.events

                -- If it exists then update the room field of the event else do nothing.
                sideEffect =
                    case eventGet of
                        Just event ->
                            let
                                updatedEv =
                                    { event | room = Just roomId }
                            in
                            Effect.sendCmd (updateEvent ( evId, updatedEv ) data.backendUrl data.token)

                        Nothing ->
                            Effect.none
            in
            ( Model data filters draggable selectedItems, sideEffect )


updateOnMenuEdit : EditMenu -> Model -> ( Model, Effect Msg )
updateOnMenuEdit msg (Model data filters draggable selectedItems) =
    case msg of
        EditEvent evID ->
            ( Model data filters draggable selectedItems, Effect.pushRoute { path = Route.Path.EditEvent_Id_ { id = String.fromInt evID }, query = Dict.empty, hash = Nothing } )

        EditLect lectID ->
            ( Model data filters draggable selectedItems, Effect.none )

        EditRoom roomID ->
            ( Model data filters draggable selectedItems, Effect.pushRoute { path = Route.Path.EditRoom_Id_ { id = String.fromInt roomID }, query = Dict.empty, hash = Nothing } )

        EditBlock blockID ->
            ( Model data filters draggable selectedItems, Effect.none )

        AddEvent ->
            ( Model data filters draggable selectedItems, Effect.pushRoute { path = Route.Path.AddEvent, query = Dict.empty, hash = Nothing } )

        AddLect ->
            ( Model data filters draggable selectedItems, Effect.none )

        AddRoom ->
            ( Model data filters draggable selectedItems, Effect.pushRoute { path = Route.Path.AddRoom, query = Dict.empty, hash = Nothing } )

        AddBlock ->
            ( Model data filters draggable selectedItems, Effect.none )



------------------------ HTTP ------------------------


updateEvent : ( EventID, Event ) -> String -> Token -> Cmd Msg
updateEvent ( id, event ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putEvent (Just id) event)
        , expect = Http.expectWhatever (handleResponse ( id, event ))
        , timeout = Nothing
        , tracker = Nothing
        }


{-| When we update an event there are 2 possible options:

1.  The event is updated successfully, in this case we do a GET request to get the updated event
2.  The event is not updated, in this case we do nothing

-}
handleResponse : ( EventID, Event ) -> Result Http.Error () -> RenderMain.Msg.Msg
handleResponse ( evID, ev ) response =
    case response of
        Ok _ ->
            RenderMain.Msg.UpdateEvent (Ok ( evID, ev ))

        Err err ->
            RenderMain.Msg.UpdateEvent (Err err)
