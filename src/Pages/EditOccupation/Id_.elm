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
import ScheduleObjects.Occupation exposing (..)
import ScheduleObjects.WeekTime exposing (..)
import Select exposing (..)
import SelectLists.Hour exposing (..)
import SelectLists.WeekDay exposing (..)
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


type alias Model =
    { occupationID : OccupationID
    , occupation : Occupation
    , weekdayList : WeekDayList
    , hourStartList : HourList
    , hourEndList : HourList
    , token : Token
    , backendUrl : String
    , errorMsg : String
    , deleteConfirmation : Bool
    }


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
    ( Model id occupation (initWeekDayList occupation.start_time.weekday) (initHourList startHour "StartHour") (initHourList endHour "EndHour") data.token data.backendUrl "" False
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
update msg model =
    case msg of
        SelectWeekday selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.weekdayList.selectState

                newWeekday =
                    case maybeAction of
                        Just (Select weekday) ->
                            weekday

                        _ ->
                            model.occupation.start_time.weekday

                oldStart_time =
                    model.occupation.start_time

                oldEnd_time =
                    model.occupation.end_time
            in
            ( model
                |> setOccupation
                    (model.occupation
                        |> setOccStartTime { oldStart_time | weekday = newWeekday }
                        |> setOccEndTime { oldEnd_time | weekday = newWeekday }
                    )
                |> setWeekdayList (setWeekdaySelectState updatedSelectState model.weekdayList |> setWeekdayListSelect newWeekday)
            , Effect.sendCmd (Cmd.map SelectWeekday selectCmds)
            )

        SelectStartHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.hourStartList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( model.occupation.start_time.hour, model.occupation.start_time.minute )

                oldStart_time =
                    model.occupation.start_time

                newStart_time =
                    { oldStart_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( model
                |> setOccupation
                    (model.occupation
                        |> setOccStartTime newStart_time
                    )
                |> setStartHourList (setWeekdaySelectState updatedSelectState model.hourStartList |> setHourListSelect newHour)
            , Effect.sendCmd (Cmd.map SelectStartHour selectCmds)
            )

        SelectEndHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.hourEndList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( model.occupation.end_time.hour, model.occupation.end_time.minute )

                oldEnd_time =
                    model.occupation.end_time

                newEnd_time =
                    { oldEnd_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( model
                |> setOccupation
                    (model.occupation
                        |> setOccEndTime newEnd_time
                    )
                |> setEndHourList (setWeekdaySelectState updatedSelectState model.hourEndList |> setHourListSelect newHour)
            , Effect.sendCmd (Cmd.map SelectEndHour selectCmds)
            )

        UpdateOccupationRequest ->
            ( model, Effect.sendCmd (updateOccupation ( model.occupationID, model.occupation ) model.backendUrl model.token) )

        DeleteOccupationRequest ->
            if model.deleteConfirmation then
                ( { model | deleteConfirmation = False }, Effect.sendCmd (deleteOccupation model.occupationID model.backendUrl model.token) )

            else
                ( { model | deleteConfirmation = True }, Effect.none )

        UpdateOccupationResult result ->
            case result of
                Ok ( updatedID, updatedOccupation ) ->
                    let
                        route =
                            { path = Route.Path.EditRoom_Id_ { id = String.fromInt model.occupation.room }, query = Dict.empty, hash = Nothing }
                    in
                    ( model, Effect.updateOccupation ( updatedID, updatedOccupation ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        DeleteOccupationResult result ->
            case result of
                Ok _ ->
                    let
                        route =
                            { path = Route.Path.EditRoom_Id_ { id = String.fromInt model.occupation.room }, query = Dict.empty, hash = Nothing }
                    in
                    ( model, Effect.deleteOccupation model.occupationID (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.EditRoom_Id_ { id = String.fromInt model.occupation.room }, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Editar Ocupação"
    , body =
        [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect model.weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect model.hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect model.hourEndList "Hora de Fim")
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateOccupationRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteOccupationRequest ]
            [ text
                (if model.deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
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
