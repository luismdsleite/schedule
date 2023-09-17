module Main.Update exposing (..)

import Browser.Dom exposing (Error(..))
import Decoders
import Dict
import DnD
import Effect exposing (Effect)
import Encoders
import Http
import Main.DisplayEvents exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Maybe.Extra
import Route.Path
import ScheduleObjects.Data exposing (..)
import ScheduleObjects.Event exposing (Event, EventID, setEvent, setEventRoom)
import ScheduleObjects.Filters exposing (..)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Time


{-| Update Block / Event / Room / Lecturer filters based on the msg received.
-}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ItemClick clickMsg ->
            updateOnItemClick clickMsg model

        DnDMsg dndmsg ->
            ( { model | draggable = DnD.update dndmsg model.draggable }, Effect.none )

        OnDrop ( _, weekTime ) eventID ->
            let
                event =
                    Dict.get eventID model.data.events
            in
            case event of
                Just ev ->
                    let
                        startingTime =
                            Maybe.Extra.withDefaultLazy (\() -> WeekTime Time.Mon endingHour endingMinute) ev.start_time

                        endingTime =
                            Maybe.Extra.withDefaultLazy (\() -> WeekTime Time.Mon endingHour endingMinute) ev.end_time

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
                    ( model, Effect.sendCmd (updateEvent ( eventID, newEv ) False model.data.backendUrl model.data.token) )

                Nothing ->
                    ( model, Effect.none )

        UpdateEvent result ->
            case result of
                Ok ( evID, ( ev, isHidden ) ) ->
                    let
                        newEvents =
                            if isHidden then
                                Dict.remove evID model.data.events

                            else
                                Dict.insert evID ev model.data.events

                        newHiddenEvents =
                            if isHidden then
                                Dict.insert evID ev model.data.hiddenEvents

                            else
                                Dict.remove evID model.data.hiddenEvents

                        updatedSelectedItems =
                            -- If the timeslots for the event we are searching for available rooms are modified, we must update the 'selectedItems.event' variable.
                            case model.selectedItems.event of
                                Just ( selectedId, _ ) ->
                                    if selectedId == evID && not isHidden then
                                        model.selectedItems |> setSelectedEvent (Just ( selectedId, ev ))

                                    else
                                        model.selectedItems

                                Nothing ->
                                    model.selectedItems
                    in
                    ( model
                        |> setData (model.data |> setDataEvents newEvents |> setDataHiddenEvents newHiddenEvents)
                        |> setSelectedItems updatedSelectedItems
                    , Effect.updateEvent ( evID, ( ev, isHidden ) ) Nothing
                    )

                Err _ ->
                    ( model, Effect.none )

        EditMenu menuMsg ->
            updateOnMenuEdit menuMsg model


updateOnItemClick : OnItemClick -> Model -> ( Model, Effect Msg )
updateOnItemClick msg model =
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

        createNewLectFilter : Int -> Int -> Event -> Bool
        createNewLectFilter lectid _ event =
            case event.lecturer of
                Just id ->
                    id == lectid

                Nothing ->
                    False

        createNewOccFilter : ID -> OccupationID -> Occupation -> Bool
        createNewOccFilter roomId _ occ =
            roomId == occ.room

        createNewRestFilter : ID -> RestrictionID -> Restriction -> Bool
        createNewRestFilter lectId _ rest =
            lectId == rest.lect
    in
    case msg of
        OnBlockClick ( blockId, block ) ->
            ( model
                |> setFilters (model.filters |> setBlockFilter block.cond)
                |> setSelectedItems (model.selectedItems |> setSelectedBlock (Just ( blockId, block )))
            , Effect.none
            )

        -- Get all events with a certain Room ID and with it update the Room Filter and Abbr.
        OnRoomClick ( id, room ) ->
            ( model
                |> setFilters (model.filters |> setRoomFilter (createNewRoomFilter id) |> setOccFilter (createNewOccFilter id))
                |> setSelectedItems (model.selectedItems |> setSelectedRoom (Just ( id, room )))
            , Effect.none
            )

        -- Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
        OnLecturerClick ( id, lect ) ->
            ( model
                |> setFilters (model.filters |> setLectFilter (createNewLectFilter id) |> setRestFilter (createNewRestFilter id))
                |> setSelectedItems (model.selectedItems |> setSelectedLect (Just ( id, lect )))
            , Effect.none
            )

        {-
           For an Event click we need to change both the room Filter and the Lecturer Filter.
           Get all events with a certain Room ID and with it update the Room Filter.
           Get all events with a certain Lecturer ID and with it update the Lecturer Filter.
           If the event ID received is not valid then no changes will occur.
        -}
        OnEventClick ( evID, ev ) ->
            let
                -- Updating Room & Occupation Filters + Selected Room
                ( updatedRoomFilter, updatedOccupationsFilter, updatedSelectedRoom ) =
                    case ev.room of
                        Just roomId ->
                            case Dict.get roomId model.data.rooms of
                                Just room ->
                                    ( createNewRoomFilter roomId, createNewOccFilter roomId, Just ( roomId, room ) )

                                Nothing ->
                                    ( model.filters.room, model.filters.occupations, model.selectedItems.room )

                        Nothing ->
                            ( model.filters.room, model.filters.occupations, model.selectedItems.room )

                -- Updating Lecturer & Restriction Filters + Selected Lecturer
                ( updatedLectFilter, updatedRestrictionsFilter, updatedSelectedLect ) =
                    case ev.lecturer of
                        Just lectId ->
                            case Dict.get lectId model.data.lecturers of
                                Just lect ->
                                    ( createNewLectFilter lectId, createNewRestFilter lectId, Just ( lectId, lect ) )

                                Nothing ->
                                    ( model.filters.lect, model.filters.restrictions, model.selectedItems.lect )

                        Nothing ->
                            ( model.filters.lect, model.filters.restrictions, model.selectedItems.lect )
            in
            ( model
                |> setFilters
                    (model.filters
                        |> setRoomFilter updatedRoomFilter
                        |> setLectFilter updatedLectFilter
                        |> setOccFilter updatedOccupationsFilter
                        |> setRestFilter updatedRestrictionsFilter
                    )
                |> setSelectedItems
                    (model.selectedItems
                        |> setSelectedEvent (Just ( evID, ev ))
                        |> setSelectedRoom updatedSelectedRoom
                        |> setSelectedLect updatedSelectedLect
                    )
            , Effect.none
            )

        ChangeEventRoomClick evId roomId ->
            let
                eventGet =
                    Dict.get evId model.data.events

                -- If it exists then update the room field of the event else do nothing.
                sideEffect =
                    case eventGet of
                        Just event ->
                            Effect.sendCmd (updateEvent ( evId, event |> setEventRoom (Just roomId) ) False model.data.backendUrl model.data.token)

                        Nothing ->
                            Effect.none
            in
            ( model, sideEffect )


updateOnMenuEdit : EditMenu -> Model -> ( Model, Effect Msg )
updateOnMenuEdit msg model =
    case msg of
        EditEvent evID ->
            ( model, Effect.pushRoute { path = Route.Path.EditEvent_Id_ { id = String.fromInt evID }, query = Dict.empty, hash = Nothing } )

        EditLect lectID ->
            ( model, Effect.pushRoute { path = Route.Path.EditLect_Id_ { id = String.fromInt lectID }, query = Dict.empty, hash = Nothing } )

        EditRoom roomID ->
            ( model, Effect.pushRoute { path = Route.Path.EditRoom_Id_ { id = String.fromInt roomID }, query = Dict.empty, hash = Nothing } )

        EditBlock blockID ->
            ( model, Effect.pushRoute { path = Route.Path.EditBlock_Id_ { id = String.fromInt blockID }, query = Dict.empty, hash = Nothing } )

        AddEvent ->
            ( model, Effect.pushRoute { path = Route.Path.AddEvent, query = Dict.empty, hash = Nothing } )

        AddLect ->
            ( model, Effect.pushRoute { path = Route.Path.AddLect, query = Dict.empty, hash = Nothing } )

        AddRoom ->
            ( model, Effect.pushRoute { path = Route.Path.AddRoom, query = Dict.empty, hash = Nothing } )

        AddBlock ->
            ( model, Effect.pushRoute { path = Route.Path.AddBlock, query = Dict.empty, hash = Nothing } )



------------------------ HTTP ------------------------


updateEvent : ( EventID, Event ) -> IsHidden -> String -> Token -> Cmd Msg
updateEvent ( id, event ) isHidden backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putEvent (Just id) event isHidden)
        , expect = Http.expectJson Main.Msg.UpdateEvent (Decoders.responseParser Decoders.getEventAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
