module Pages.AddEvent exposing (Model, Msg, page)

import Decoders
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Styled
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Event exposing (Event, EventID, asEventIn, setEvent, setEventAbbr, setEventEndTime, setEventLecturer, setEventRoom, setEventStartTime, setEventSubject)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.Room exposing (RoomID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.WeekTimeConverters exposing (..)
import Select exposing (..)
import SelectLists.Hour exposing (MaybeHourList, initHourList, renderMaybeHourSelect, setEndHourList, setHourListSelect, setHourListSelectState, setStartHourList)
import SelectLists.WeekDay exposing (MaybeWeekDayList, initWeekDayList, renderMaybeWeekdaySelect, setWeekdayList, setWeekdayListSelect, setWeekdaySelectState)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { data : Data
    , event : Event
    , weekdayList : MaybeWeekDayList
    , hourStartList : MaybeHourList
    , hourEndList : MaybeHourList
    , roomList : AbbrList
    , lectList : AbbrList
    , errorMsg : String
    , isHidden : Bool
    }


type alias AbbrList =
    { selectState : Select.State
    , items : List (Select.MenuItem ID)
    , selectedItem : Maybe ID
    }


setRoomList : AbbrList -> { a | roomList : AbbrList } -> { a | roomList : AbbrList }
setRoomList roomList a =
    { a | roomList = roomList }


setLectList : AbbrList -> { a | lectList : AbbrList } -> { a | lectList : AbbrList }
setLectList lectList a =
    { a | lectList = lectList }


setAbbrListSelectState : Select.State -> AbbrList -> AbbrList
setAbbrListSelectState selectState abbrList =
    { abbrList | selectState = selectState }


setAbbrListSelect : Maybe ID -> AbbrList -> AbbrList
setAbbrListSelect selectedItem abbrList =
    { abbrList | selectedItem = selectedItem }



-- INIT


init : Data -> () -> ( Model, Effect Msg )
init data () =
    let
        event =
            Event "" "" Nothing Nothing Nothing Nothing
    in
    ( Model data event (initWeekDayList Nothing) (initHourList Nothing "StartHour") (initHourList Nothing "EndHour") (initAbbrList event.room (Dict.toList data.rooms) "Room") (initAbbrList event.lecturer (Dict.toList data.lecturers) "Lecturer") "" False
    , Effect.none
    )


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
    | SelectRoom (Select.Msg RoomID)
    | SelectLect (Select.Msg LecturerID)
    | UpdateEventRequest
    | UpdateEventResult (Result Http.Error ( EventID, ( Event, IsHidden ) ))
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    -- update msg (Model data ev weekdayList hourStartList hourEndList roomList lectList errorMsg isHidden) =
    case msg of
        SubjectAbbrChange str ->
            ( setEventAbbr str model.event |> asEventIn model, Effect.none )

        SubjectChange str ->
            ( setEventSubject str model.event |> asEventIn model, Effect.none )

        SelectWeekday selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.weekdayList.selectState

                newModel =
                    case maybeAction of
                        Just (Select weekday) ->
                            let
                                newStart_time =
                                    case model.event.start_time of
                                        Just start ->
                                            { start | weekday = weekday }

                                        Nothing ->
                                            { weekday = weekday, hour = 8, minute = 0 }

                                newEnd_time =
                                    case model.event.end_time of
                                        Just end ->
                                            { end | weekday = weekday }

                                        Nothing ->
                                            { weekday = weekday, hour = 9, minute = 0 }
                            in
                            model
                                |> setEvent (setEventStartTime (Just newStart_time) model.event |> setEventEndTime (Just newEnd_time))
                                |> setWeekdayList (setWeekdaySelectState updatedSelectState model.weekdayList |> setWeekdayListSelect (Just weekday))
                                |> setStartHourList (setHourListSelect (Just ( newStart_time.hour, newStart_time.minute )) model.hourStartList)
                                |> setEndHourList (setHourListSelect (Just ( newEnd_time.hour, newEnd_time.minute )) model.hourEndList)

                        Just Clear ->
                            model
                                |> setEvent (setEventStartTime Nothing model.event |> setEventEndTime Nothing)
                                |> setWeekdayList (setWeekdaySelectState updatedSelectState model.weekdayList |> setWeekdayListSelect Nothing)
                                |> setStartHourList (setHourListSelect Nothing model.hourStartList)
                                |> setEndHourList (setHourListSelect Nothing model.hourEndList)

                        _ ->
                            model
                                |> setWeekdayList (setWeekdaySelectState updatedSelectState model.weekdayList)
            in
            ( newModel
            , Effect.sendCmd (Cmd.map SelectWeekday selectCmds)
            )

        SelectStartHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.hourStartList.selectState

                newModel =
                    case maybeAction of
                        Just (Select ( hour, minute )) ->
                            let
                                ( newStartTime, newEndTime ) =
                                    case model.event.start_time of
                                        Just start ->
                                            ( Just { start | hour = hour, minute = minute }, model.event.end_time )

                                        Nothing ->
                                            ( Just (WeekTime Time.Mon hour minute), Just (WeekTime Time.Mon 19 30) )
                            in
                            model
                                |> setEvent (setEventStartTime newStartTime model.event |> setEventEndTime newEndTime)
                                |> setStartHourList (setHourListSelectState updatedSelectState model.hourStartList |> setHourListSelect (Just ( hour, minute )))

                        _ ->
                            model
                                |> setStartHourList (setHourListSelectState updatedSelectState model.hourStartList)
            in
            ( newModel
            , Effect.sendCmd (Cmd.map SelectStartHour selectCmds)
            )

        SelectEndHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.hourEndList.selectState

                newModel =
                    case maybeAction of
                        Just (Select ( hour, minute )) ->
                            let
                                ( newStartTime, newEndTime ) =
                                    case model.event.end_time of
                                        Just end ->
                                            ( model.event.start_time, Just { end | hour = hour, minute = minute } )

                                        Nothing ->
                                            ( Just (WeekTime Time.Mon 8 0), Just (WeekTime Time.Mon hour minute) )
                            in
                            model
                                |> setEvent (setEventStartTime newStartTime model.event |> setEventEndTime newEndTime)
                                |> setEndHourList (setHourListSelectState updatedSelectState model.hourEndList |> setHourListSelect (Just ( hour, minute )))

                        _ ->
                            model
                                |> setEndHourList (setHourListSelectState updatedSelectState model.hourEndList)
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

        SelectRoom selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.roomList.selectState

                newModel =
                    case maybeAction of
                        Just (Select roomID) ->
                            model
                                |> setEvent (setEventRoom (Just roomID) model.event)
                                |> setRoomList (setAbbrListSelectState updatedSelectState model.roomList |> setAbbrListSelect (Just roomID))

                        Just Clear ->
                            model
                                |> setEvent (setEventRoom Nothing model.event)
                                |> setRoomList (setAbbrListSelectState updatedSelectState model.roomList |> setAbbrListSelect Nothing)

                        _ ->
                            model
                                |> setRoomList (setAbbrListSelectState updatedSelectState model.roomList)
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        SelectLect selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.lectList.selectState

                newModel =
                    case maybeAction of
                        Just (Select lectID) ->
                            model
                                |> setEvent (setEventLecturer (Just lectID) model.event)
                                |> setLectList (setAbbrListSelectState updatedSelectState model.lectList |> setAbbrListSelect (Just lectID))

                        Just Clear ->
                            model
                                |> setEvent (setEventLecturer Nothing model.event)
                                |> setLectList (setAbbrListSelectState updatedSelectState model.lectList |> setAbbrListSelect Nothing)

                        _ ->
                            model
                                |> setLectList (setAbbrListSelectState updatedSelectState model.lectList)
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        UpdateEventRequest ->
            ( model, Effect.sendCmd (updateEvent model.event model.isHidden model.data.backendUrl model.data.token) )

        UpdateEventResult result ->
            case result of
                Ok idAndEvent ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( model, Effect.updateEvent idAndEvent (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        VisibilityChange changeVisibility ->
            ( { model | isHidden = changeVisibility }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Criar Cadeira"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.event.subjectAbbr, onInput SubjectAbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.event.subject, onInput SubjectChange, Html.Attributes.placeholder "Nome Da Cadeira" ] []
        , Html.map SelectWeekday (Html.Styled.toUnstyled <| renderMaybeWeekdaySelect model.weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderMaybeHourSelect model.hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderMaybeHourSelect model.hourEndList "Hora de Fim")
        , Html.map SelectRoom (Html.Styled.toUnstyled <| renderAbbrSelect model.roomList model.data.rooms "Sala")
        , Html.map SelectLect (Html.Styled.toUnstyled <| renderAbbrSelect model.lectList model.data.lecturers "Docente")
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Cadeira" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ style "margin-left" "2%", class "button", onClick UpdateEventRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
        ]
    }


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
            |> Select.clearable True
            |> Select.state itemList.selectState
            |> Select.menuItems itemList.items
            |> Select.placeholder placeholder
        )



-- HTTP


updateEvent : Event -> IsHidden -> String -> Token -> Cmd Msg
updateEvent event isHidden backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events"
        , body = Http.jsonBody (Encoders.putEvent Nothing event isHidden)
        , expect = Http.expectJson UpdateEventResult (Decoders.responseParser Decoders.getEventAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
