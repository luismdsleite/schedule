module Pages.EditEvent.Id_ exposing (Model, Msg, page)

import Decoders
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Styled
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.WeekTimeConverters exposing (..)
import Select exposing (..)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route.params.id
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Model Data ( EventID, Event ) WeekDayList HourList HourList AbbrList AbbrList Bool String


type alias WeekDayList =
    { selectState : Select.State
    , items : List (Select.MenuItem Time.Weekday)
    , selectedWeekday : Maybe Time.Weekday
    }


type alias HourList =
    { selectState : Select.State
    , items : List (Select.MenuItem ( Int, Int ))
    , selectedHour : Maybe ( Int, Int )
    }


type alias AbbrList =
    { selectState : Select.State
    , items : List (Select.MenuItem ID)
    , selectedItem : Maybe ID
    }


init : Data -> String -> () -> ( Model, Effect Msg )
init data eventIDParam () =
    let
        eventID =
            case String.toInt eventIDParam of
                Just number ->
                    number

                Nothing ->
                    -1

        event =
            Dict.get eventID data.events
                |> Maybe.withDefault (Event "" "" Nothing Nothing Nothing Nothing)

        weekDay =
            event.start_time |> Maybe.map (\a -> a.weekday)

        startHour =
            event.start_time |> Maybe.map (\a -> ( a.hour, a.minute ))

        endHour =
            event.end_time |> Maybe.map (\a -> ( a.hour, a.minute ))
    in
    ( Model data ( eventID, event ) (initWeekDayList weekDay) (initHourList startHour "StartHour") (initHourList endHour "EndHour") (initAbbrList event.room (Dict.toList data.rooms) "Room") (initAbbrList event.lecturer (Dict.toList data.lecturers) "Lecturer") False "", Effect.none )


initWeekDayList : Maybe Time.Weekday -> WeekDayList
initWeekDayList selectedWeekday =
    { selectState =
        Select.initState (Select.selectIdentifier "Weekday")
    , items =
        List.map (\weekday -> basicMenuItem { item = weekday, label = toPortugueseWeekday weekday }) displayedWeekDays
    , selectedWeekday = selectedWeekday
    }


initHourList : Maybe ( Int, Int ) -> String -> HourList
initHourList eventHour selectIdentifier =
    let
        hours =
            computeTimeSlots (WeekTime Time.Mon 8 0) (WeekTime Time.Mon 20 0) []
                |> List.map (\a -> ( a.hour, a.minute ))
                |> List.reverse
    in
    { selectState =
        Select.initState (Select.selectIdentifier selectIdentifier)
    , items =
        List.map (\( hour, minute ) -> basicMenuItem { item = ( hour, minute ), label = convertHourAndMinute hour minute }) hours
    , selectedHour = eventHour
    }


initAbbrList : a -> List ( b, { c | abbr : String } ) -> String -> { selectState : State, items : List (MenuItem b), selectedItem : a }
initAbbrList selectedID list selectIdentifier =
    { selectState =
        Select.initState (Select.selectIdentifier selectIdentifier)
    , items =
        List.map (\( abbrID, room ) -> basicMenuItem { item = abbrID, label = room.abbr }) list
    , selectedItem = selectedID
    }



-- UPDATE


type Msg
    = SubjectAbbrChange String
    | SubjectChange String
    | SelectWeekday (Select.Msg Time.Weekday)
    | SelectStartHour (Select.Msg ( Int, Int ))
    | SelectEndHour (Select.Msg ( Int, Int ))
    | ClearTime
    | SelectRoom (Select.Msg RoomID)
    | ClearRoom
    | SelectLect (Select.Msg LecturerID)
    | ClearLect
    | UpdateEventRequest
    | UpdateEventResult (Result Http.Error ( EventID, Event ))
    | DeleteEventRequest
    | DeleteEventResult (Result Http.Error ())
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg) =
    case msg of
        SubjectAbbrChange str ->
            ( Model data ( evId, { ev | subjectAbbr = str } ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.none )

        SubjectChange str ->
            ( Model data ( evId, { ev | subject = str } ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.none )

        SelectWeekday selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg weekdayList.selectState

                newModel =
                    case maybeAction of
                        Just (Select weekday) ->
                            let
                                newStart_time =
                                    case ev.start_time of
                                        Just start ->
                                            { start | weekday = weekday }

                                        Nothing ->
                                            { weekday = weekday, hour = 8, minute = 0 }

                                newEnd_time =
                                    case ev.end_time of
                                        Just end ->
                                            { end | weekday = weekday }

                                        Nothing ->
                                            { weekday = weekday, hour = 9, minute = 0 }
                            in
                            Model data ( evId, { ev | start_time = Just newStart_time, end_time = Just newEnd_time } ) { weekdayList | selectedWeekday = Just weekday, selectState = updatedSelectState } { hourStartList | selectedHour = Just ( newStart_time.hour, newStart_time.minute ) } { hourEndList | selectedHour = Just ( newEnd_time.hour, newEnd_time.minute ) } roomList lectList deleteConfirmation errorMsg

                        _ ->
                            Model data ( evId, ev ) { weekdayList | selectState = updatedSelectState } hourStartList hourEndList roomList lectList deleteConfirmation errorMsg
            in
            ( newModel
            , Effect.sendCmd (Cmd.map SelectWeekday selectCmds)
            )

        SelectStartHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourStartList.selectState

                newModel =
                    case maybeAction of
                        Just (Select ( hour, minute )) ->
                            let
                                ( newStartTime, newEndTime ) =
                                    case ev.start_time of
                                        Just start ->
                                            ( Just { start | hour = hour, minute = minute }, ev.end_time )

                                        Nothing ->
                                            ( Just (WeekTime Time.Mon hour minute), Just (WeekTime Time.Mon 19 30) )
                            in
                            Model data ( evId, { ev | start_time = newStartTime, end_time = newEndTime } ) weekdayList { hourStartList | selectState = updatedSelectState, selectedHour = Just ( hour, minute ) } hourEndList roomList lectList deleteConfirmation errorMsg

                        _ ->
                            Model data ( evId, ev ) weekdayList { hourStartList | selectState = updatedSelectState } hourEndList roomList lectList deleteConfirmation errorMsg
            in
            ( newModel
            , Effect.sendCmd (Cmd.map SelectStartHour selectCmds)
            )

        SelectEndHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourEndList.selectState

                newModel =
                    case maybeAction of
                        Just (Select ( hour, minute )) ->
                            let
                                ( newStartTime, newEndTime ) =
                                    case ev.end_time of
                                        Just end ->
                                            ( ev.start_time, Just { end | hour = hour, minute = minute } )

                                        Nothing ->
                                            ( Just (WeekTime Time.Mon 8 0), Just (WeekTime Time.Mon hour minute) )
                            in
                            Model data ( evId, { ev | start_time = newStartTime, end_time = newEndTime } ) weekdayList hourStartList { hourEndList | selectState = updatedSelectState, selectedHour = Just ( hour, minute ) } roomList lectList deleteConfirmation errorMsg

                        _ ->
                            Model data ( evId, ev ) weekdayList hourStartList { hourEndList | selectState = updatedSelectState } roomList lectList deleteConfirmation errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

        SelectRoom selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg roomList.selectState

                newModel =
                    case maybeAction of
                        Just (Select roomID) ->
                            Model data ( evId, { ev | room = Just roomID } ) weekdayList hourStartList hourEndList { roomList | selectState = updatedSelectState, selectedItem = Just roomID } lectList deleteConfirmation errorMsg

                        _ ->
                            Model data ( evId, ev ) weekdayList hourStartList hourEndList { roomList | selectState = updatedSelectState } lectList deleteConfirmation errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        ClearRoom ->
            ( Model data ( evId, { ev | room = Nothing } ) weekdayList hourStartList hourEndList { roomList | selectedItem = Nothing } lectList deleteConfirmation errorMsg, Effect.none )

        SelectLect selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg lectList.selectState

                newModel =
                    case maybeAction of
                        Just (Select lectID) ->
                            Model data ( evId, { ev | lecturer = Just lectID } ) weekdayList hourStartList hourEndList roomList { lectList | selectState = updatedSelectState, selectedItem = Just lectID } deleteConfirmation errorMsg

                        _ ->
                            Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList { lectList | selectState = updatedSelectState } deleteConfirmation errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        ClearLect ->
            ( Model data ( evId, { ev | lecturer = Nothing } ) weekdayList hourStartList hourEndList roomList { lectList | selectedItem = Nothing } deleteConfirmation errorMsg, Effect.none )

        ClearTime ->
            ( Model data ( evId, { ev | start_time = Nothing, end_time = Nothing } ) { weekdayList | selectedWeekday = Nothing } { hourStartList | selectedHour = Nothing } { hourEndList | selectedHour = Nothing } roomList lectList deleteConfirmation errorMsg, Effect.none )

        UpdateEventRequest ->
            ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.sendCmd (updateEvent ( evId, ev ) data.backendUrl data.token) )

        UpdateEventResult result ->
            case result of
                Ok ( id, event ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model { data | events = Dict.insert id event data.events } ( id, event ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.updateEvent ( id, event ) (Just route) )

                Err err ->
                    ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation (Decoders.errorToString err), Effect.none )

        DeleteEventRequest ->
            if deleteConfirmation then
                ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.sendCmd (deleteEvent evId data.backendUrl data.token) )

            else
                ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList True errorMsg, Effect.none )

        DeleteEventResult result ->
            case result of
                Ok _ ->
                    ( Model { data | events = Dict.remove evId data.events } ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.deleteEvent evId (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err err ->
                    ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model data ( evId, ev ) weekdayList hourStartList hourEndList roomList lectList deleteConfirmation errorMsg) =
    { title = "Editar Cadeira"
    , body =
        [ input [ class "input-box", style "width" "100%", value ev.subjectAbbr, onInput SubjectAbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value ev.subject, onInput SubjectChange, Html.Attributes.placeholder "Nome Da Cadeira" ] []
        , div [ style "width" "100%", style "display" "grid", style "grid-template-columns" "95% 5%" ] [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect weekdayList), div [ class "gg-remove", onClick ClearTime ] [] ]
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect hourEndList "Hora de Fim")
        , div [ style "width" "100%", style "display" "grid", style "grid-template-columns" "95% 5%" ] [ Html.map SelectRoom (Html.Styled.toUnstyled <| renderAbbrSelect roomList data.rooms "Sala"), div [ class "gg-remove", onClick ClearRoom ] [] ]
        , div [ style "width" "100%", style "display" "grid", style "grid-template-columns" "95% 5%" ] [ Html.map SelectLect (Html.Styled.toUnstyled <| renderAbbrSelect lectList data.lecturers "Docente"), div [ class "gg-remove", onClick ClearLect ] [] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateEventRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteEventRequest ]
            [ text
                (if deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }


renderWeekdaySelect : WeekDayList -> Html.Styled.Html (Select.Msg Time.Weekday)
renderWeekdaySelect weekdayList =
    Select.view
        ((Select.single <| Maybe.map (\weekday -> basicMenuItem { item = weekday, label = toPortugueseWeekday weekday }) weekdayList.selectedWeekday)
            |> Select.state weekdayList.selectState
            |> Select.menuItems weekdayList.items
            |> Select.placeholder "Dia da Semana"
        )


renderHourSelect : HourList -> String -> Html.Styled.Html (Select.Msg ( Int, Int ))
renderHourSelect hourList placeholder =
    Select.view
        ((Select.single <| Maybe.map (\( hour, minute ) -> basicMenuItem { item = ( hour, minute ), label = convertHourAndMinute hour minute }) hourList.selectedHour)
            |> Select.state hourList.selectState
            |> Select.menuItems hourList.items
            |> Select.placeholder placeholder
        )


getAbbr : comparable -> Dict comparable { a | abbr : String } -> String
getAbbr abbrID rooms =
    case Dict.get abbrID rooms of
        Just room ->
            room.abbr

        Nothing ->
            ""


renderAbbrSelect itemList items placeholder =
    Select.view
        ((Select.single <| Maybe.map (\abbrID -> basicMenuItem { item = abbrID, label = getAbbr abbrID items }) itemList.selectedItem)
            |> Select.state itemList.selectState
            |> Select.menuItems itemList.items
            |> Select.placeholder placeholder
        )



------------------------ HTTP ------------------------


updateEvent : ( EventID, Event ) -> String -> Token -> Cmd Msg
updateEvent ( id, event ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putEvent Nothing event)
        , expect = Http.expectWhatever (handleUpdateResponse ( id, event ))
        , timeout = Nothing
        , tracker = Nothing
        }


{-| When we update an event there are 2 possible options:

1.  The event is updated successfully, in this case we do a GET request to get the updated event
2.  The event is not updated, in this case we do nothing

-}
handleUpdateResponse : ( EventID, Event ) -> Result Http.Error () -> Msg
handleUpdateResponse ( evID, ev ) response =
    case response of
        Ok _ ->
            UpdateEventResult (Ok ( evID, ev ))

        Err err ->
            UpdateEventResult (Err err)


deleteEvent : EventID -> String -> Token -> Cmd Msg
deleteEvent id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteEventResult (Ok ())

        Err err ->
            DeleteEventResult (Err err)
