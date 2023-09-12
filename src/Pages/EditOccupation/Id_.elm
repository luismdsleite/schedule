module Pages.EditOccupation.Id_ exposing (Model, Msg, page)

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
    = Model ( OccupationID, Occupation ) WeekDayList HourList HourList String String String Bool


init : Data -> String -> () -> ( Model, Effect Msg )
init data idParam () =
    let
        id =
            String.toInt idParam
                |> Maybe.Extra.withDefaultLazy (\() -> -1)

        occupation =
            Dict.get id data.occupations |> Maybe.Extra.withDefaultLazy (\() -> Occupation -1 (WeekTime Time.Mon 9 0) (WeekTime Time.Mon 10 0))

        startHour =
            ( occupation.start_time.hour, occupation.start_time.minute )

        endHour =
            ( occupation.end_time.hour, occupation.end_time.minute )
    in
    ( Model ( id, occupation ) (initWeekDayList occupation.start_time.weekday) (initHourList startHour "StartHour") (initHourList endHour "EndHour") data.token data.backendUrl "" False
    , Effect.none
    )



-- UPDATE


type Msg
    = SelectWeekday (Select.Msg Time.Weekday)
    | SelectStartHour (Select.Msg ( Int, Int ))
    | SelectEndHour (Select.Msg ( Int, Int ))
    | UpdateOccupationRequest
    | UpdateOccupationResult (Result Http.Error ( OccupationID, Occupation ))
    | DeleteOccupationRequest
    | DeleteOccupationResult (Result Http.Error ())
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation) =
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
            ( Model ( id, { occupation | start_time = { oldStart_time | weekday = newWeekday }, end_time = { oldEnd_time | weekday = newWeekday } } ) { weekdayList | selectedWeekday = newWeekday, selectState = updatedSelectState } hourStartList hourEndList token backendUrl errorMsg deleteConfirmation, Effect.sendCmd (Cmd.map SelectWeekday selectCmds) )

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
            ( Model ( id, { occupation | start_time = newStart_time } ) weekdayList { hourStartList | selectedHour = newHour, selectState = updatedSelectState } hourEndList token backendUrl errorMsg deleteConfirmation, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

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
            ( Model ( id, { occupation | end_time = newEnd_time } ) weekdayList hourStartList { hourEndList | selectedHour = newHour, selectState = updatedSelectState } token backendUrl errorMsg deleteConfirmation, Effect.sendCmd (Cmd.map SelectEndHour selectCmds) )

        UpdateOccupationRequest ->
            ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation, Effect.sendCmd (updateOccupation ( id, occupation ) backendUrl token) )

        DeleteOccupationRequest ->
            if deleteConfirmation then
                ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg False, Effect.sendCmd (deleteOccupation id backendUrl token) )

            else
                ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg True, Effect.none )

        UpdateOccupationResult result ->
            case result of
                Ok ( updatedID, updatedOccupation ) ->
                    let
                        route =
                            { path = Route.Path.EditRoom_Id_ { id = String.fromInt occupation.room }, query = Dict.empty, hash = Nothing }
                    in
                    ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation, Effect.updateOccupation ( updatedID, updatedOccupation ) (Just route) )

                Err err ->
                    ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl (Decoders.errorToString err) deleteConfirmation, Effect.none )

        DeleteOccupationResult result ->
            case result of
                Ok _ ->
                    let
                        route =
                            { path = Route.Path.EditRoom_Id_ { id = String.fromInt occupation.room }, query = Dict.empty, hash = Nothing }
                    in
                    ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation, Effect.deleteOccupation id (Just route) )

                Err err ->
                    ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl (Decoders.errorToString err) deleteConfirmation, Effect.none )

        Return ->
            ( Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation, Effect.pushRoute { path = Route.Path.EditRoom_Id_ { id = String.fromInt occupation.room }, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model ( id, occupation ) weekdayList hourStartList hourEndList token backendUrl errorMsg deleteConfirmation) =
    { title = "Editar Ocupação"
    , body =
        [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect hourEndList "Hora de Fim")
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateOccupationRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteOccupationRequest ]
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



------------------------ HTTP ------------------------


updateOccupation : ( OccupationID, Occupation ) -> String -> Token -> Cmd Msg
updateOccupation ( id, occupation ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "occupations\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putOccupation Nothing occupation)
        , expect = Http.expectJson UpdateOccupationResult (Decoders.responseParser Decoders.getOccupationAndId)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteOccupation : OccupationID -> String -> Token -> Cmd Msg
deleteOccupation id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "occupations\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteOccupationResult (Ok ())

        Err err ->
            DeleteOccupationResult (Err err)
