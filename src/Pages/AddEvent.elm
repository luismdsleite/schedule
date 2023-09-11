module Pages.AddEvent exposing (Model, Msg, page)

import Decoders
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Styled
import Http
import Json.Decode as JD
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


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Model Data Event WeekDayList HourList HourList AbbrList AbbrList String


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



-- INIT


init : Data -> () -> ( Model, Effect Msg )
init data () =
    let
        event =
            Event "" "" Nothing Nothing Nothing Nothing
    in
    ( Model data event (initWeekDayList Nothing) (initHourList Nothing "StartHour") (initHourList Nothing "EndHour") (initAbbrList event.room (Dict.toList data.rooms) "Room") (initAbbrList event.lecturer (Dict.toList data.lecturers) "Lecturer") ""
    , Effect.none
    )


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
    | SelectRoom (Select.Msg RoomID)
    | SelectLect (Select.Msg LecturerID)
    | UpdateEventRequest
    | UpdateEventResult (Result Http.Error ( EventID, Event ))
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model data ev weekdayList hourStartList hourEndList roomList lectList errorMsg) =
    case msg of
        SubjectAbbrChange str ->
            ( Model data { ev | subjectAbbr = str } weekdayList hourStartList hourEndList roomList lectList errorMsg, Effect.none )

        SubjectChange str ->
            ( Model data { ev | subject = str } weekdayList hourStartList hourEndList roomList lectList errorMsg, Effect.none )

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
                            Model data { ev | start_time = Just newStart_time, end_time = Just newEnd_time } { weekdayList | selectedWeekday = Just weekday, selectState = updatedSelectState } { hourStartList | selectedHour = Just ( newStart_time.hour, newStart_time.minute ) } { hourEndList | selectedHour = Just ( newEnd_time.hour, newEnd_time.minute ) } roomList lectList errorMsg

                        Just Clear ->
                            Model data { ev | start_time = Nothing, end_time = Nothing } { weekdayList | selectedWeekday = Nothing, selectState = updatedSelectState } { hourStartList | selectedHour = Nothing } { hourEndList | selectedHour = Nothing } roomList lectList errorMsg

                        _ ->
                            Model data ev { weekdayList | selectState = updatedSelectState } hourStartList hourEndList roomList lectList errorMsg
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
                            Model data { ev | start_time = newStartTime, end_time = newEndTime } weekdayList { hourStartList | selectState = updatedSelectState, selectedHour = Just ( hour, minute ) } hourEndList roomList lectList errorMsg

                        _ ->
                            Model data ev weekdayList { hourStartList | selectState = updatedSelectState } hourEndList roomList lectList errorMsg
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
                            Model data { ev | start_time = newStartTime, end_time = newEndTime } weekdayList hourStartList { hourEndList | selectState = updatedSelectState, selectedHour = Just ( hour, minute ) } roomList lectList errorMsg

                        _ ->
                            Model data ev weekdayList hourStartList { hourEndList | selectState = updatedSelectState } roomList lectList errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

        SelectRoom selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg roomList.selectState

                newModel =
                    case maybeAction of
                        Just (Select roomID) ->
                            Model data { ev | room = Just roomID } weekdayList hourStartList hourEndList { roomList | selectState = updatedSelectState, selectedItem = Just roomID } lectList errorMsg

                        Just Clear ->
                            Model data { ev | room = Nothing } weekdayList hourStartList hourEndList { roomList | selectState = updatedSelectState, selectedItem = Nothing } lectList errorMsg

                        _ ->
                            Model data ev weekdayList hourStartList hourEndList { roomList | selectState = updatedSelectState } lectList errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        SelectLect selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg lectList.selectState

                newModel =
                    case maybeAction of
                        Just (Select lectID) ->
                            Model data { ev | lecturer = Just lectID } weekdayList hourStartList hourEndList roomList { lectList | selectState = updatedSelectState, selectedItem = Just lectID } errorMsg

                        Just Clear ->
                            Model data { ev | lecturer = Nothing } weekdayList hourStartList hourEndList roomList { lectList | selectState = updatedSelectState, selectedItem = Nothing } errorMsg

                        _ ->
                            Model data ev weekdayList hourStartList hourEndList roomList { lectList | selectState = updatedSelectState } errorMsg
            in
            ( newModel, Effect.sendCmd (Cmd.map SelectRoom selectCmds) )

        UpdateEventRequest ->
            ( Model data ev weekdayList hourStartList hourEndList roomList lectList errorMsg, Effect.sendCmd (updateEvent ev data.backendUrl data.token) )

        UpdateEventResult result ->
            case result of
                Ok ( id, updatedEv ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model { data | events = Dict.insert id updatedEv data.events } ev weekdayList hourStartList hourEndList roomList lectList errorMsg, Effect.updateEvent ( id, updatedEv ) (Just route) )

                Err err ->
                    ( Model data ev weekdayList hourStartList hourEndList roomList lectList (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model data ev weekdayList hourStartList hourEndList roomList lectList errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model data ev weekdayList hourStartList hourEndList roomList lectList errorMsg) =
    { title = "Criar Cadeira"
    , body =
        [ input [ class "input-box", style "width" "100%", value ev.subjectAbbr, onInput SubjectAbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value ev.subject, onInput SubjectChange, Html.Attributes.placeholder "Nome Da Cadeira" ] []
        , Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect hourEndList "Hora de Fim")
        , Html.map SelectRoom (Html.Styled.toUnstyled <| renderAbbrSelect roomList data.rooms "Sala")
        , Html.map SelectLect (Html.Styled.toUnstyled <| renderAbbrSelect lectList data.lecturers "Docente")
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ style "margin-left" "2%", class "button", onClick UpdateEventRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }


renderWeekdaySelect : WeekDayList -> Html.Styled.Html (Select.Msg Time.Weekday)
renderWeekdaySelect weekdayList =
    Select.view
        ((Select.single <| Maybe.map (\weekday -> basicMenuItem { item = weekday, label = toPortugueseWeekday weekday }) weekdayList.selectedWeekday)
            |> Select.clearable True
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
            |> Select.clearable True
            |> Select.state itemList.selectState
            |> Select.menuItems itemList.items
            |> Select.placeholder placeholder
        )



-- HTTP


updateEvent : Event -> String -> Token -> Cmd Msg
updateEvent event backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "events"
        , body = Http.jsonBody (Encoders.putEvent Nothing event)
        , expect = Http.expectJson UpdateEventResult (Decoders.responseParser Decoders.getEventAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
