module Pages.AddOccupation exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Styled
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Select exposing (..)
import SelectLists.Hour exposing (HourList, initHourList, renderHourSelect)
import SelectLists.WeekDay exposing (WeekDayList, initWeekDayList, renderWeekdaySelect)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared (Dict.get "roomID" route.query)
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Model Occupation WeekDayList HourList HourList String String String


init : Data -> Maybe String -> () -> ( Model, Effect Msg )
init data queryRoomID () =
    let
        roomID =
            queryRoomID
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault -1

        occupation =
            Occupation roomID (WeekTime Time.Mon 9 0) (WeekTime Time.Mon 10 0)

        startHour =
            ( occupation.start_time.hour, occupation.start_time.minute )

        endHour =
            ( occupation.end_time.hour, occupation.end_time.minute )
    in
    ( Model occupation (initWeekDayList occupation.start_time.weekday) (initHourList startHour "StartHour") (initHourList endHour "EndHour") data.token data.backendUrl ""
    , Effect.none
    )



-- UPDATE


type Msg
    = SelectWeekday (Select.Msg Time.Weekday)
    | SelectStartHour (Select.Msg ( Int, Int ))
    | SelectEndHour (Select.Msg ( Int, Int ))
    | UpdateOccupationRequest
    | UpdateOccupationResult (Result Http.Error ( OccupationID, Occupation ))
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model occupation weekdayList hourStartList hourEndList token backendUrl errorMsg) =
    case msg of
        SelectWeekday selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg weekdayList.selectState

                newWeekday =
                    case maybeAction of
                        Just (Select weekday) ->
                            weekday

                        _ ->
                            occupation.start_time.weekday

                oldStart_time =
                    occupation.start_time

                oldEnd_time =
                    occupation.end_time
            in
            ( Model { occupation | start_time = { oldStart_time | weekday = newWeekday }, end_time = { oldEnd_time | weekday = newWeekday } } { weekdayList | selectedWeekday = newWeekday, selectState = updatedSelectState } hourStartList hourEndList token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectWeekday selectCmds) )

        SelectStartHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourStartList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( occupation.start_time.hour, occupation.start_time.minute )

                oldStart_time =
                    occupation.start_time

                newStart_time =
                    { oldStart_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( Model { occupation | start_time = newStart_time } weekdayList { hourStartList | selectedHour = newHour, selectState = updatedSelectState } hourEndList token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

        SelectEndHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourEndList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( occupation.end_time.hour, occupation.end_time.minute )

                oldEnd_time =
                    occupation.end_time

                newEnd_time =
                    { oldEnd_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( Model { occupation | end_time = newEnd_time } weekdayList hourStartList { hourEndList | selectedHour = newHour, selectState = updatedSelectState } token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectEndHour selectCmds) )

        UpdateOccupationRequest ->
            ( Model occupation weekdayList hourStartList hourEndList token backendUrl errorMsg, Effect.sendCmd (updateOccupation occupation backendUrl token) )

        UpdateOccupationResult result ->
            case result of
                Ok ( updatedID, updatedOccupation ) ->
                    let
                        route =
                            { path = Route.Path.EditRoom_Id_ { id = String.fromInt occupation.room }, query = Dict.empty, hash = Nothing }
                    in
                    ( Model occupation weekdayList hourStartList hourEndList token backendUrl errorMsg, Effect.updateOccupation ( updatedID, updatedOccupation ) (Just route) )

                Err err ->
                    ( Model occupation weekdayList hourStartList hourEndList token backendUrl (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model occupation weekdayList hourStartList hourEndList token backendUrl errorMsg, Effect.pushRoute { path = Route.Path.EditRoom_Id_ { id = String.fromInt occupation.room }, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model occupation weekdayList hourStartList hourEndList token backendUrl errorMsg) =
    { title = "Adicionar Ocupação"
    , body =
        [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect hourEndList "Hora de Fim")
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateOccupationRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }



------------------------ HTTP ------------------------


updateOccupation : Occupation -> String -> Token -> Cmd Msg
updateOccupation occupation backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "occupations"
        , body = Http.jsonBody (Encoders.putOccupation Nothing occupation)
        , expect = Http.expectJson UpdateOccupationResult (Decoders.responseParser Decoders.getOccupationAndId)
        , timeout = Nothing
        , tracker = Nothing
        }
