module Pages.AddRestriction exposing (Model, Msg, page)

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
import ScheduleObjects.Restriction as Restriction exposing (Category, Restriction, RestrictionID)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Select exposing (..)
import SelectLists.Category exposing (CategoryList, initCategoryList, renderCategorySelect)
import SelectLists.Hour exposing (HourList, initHourList, renderHourSelect)
import SelectLists.WeekDay exposing (WeekDayList, initWeekDayList, renderWeekdaySelect)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared (Dict.get "lectID" route.query)
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Model Restriction WeekDayList HourList HourList CategoryList String String String


init : Data -> Maybe String -> () -> ( Model, Effect Msg )
init data queryLectID () =
    let
        lectID =
            queryLectID
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault -1

        restriction =
            Restriction lectID (WeekTime Time.Mon 9 0) (WeekTime Time.Mon 10 0) Restriction.Preference

        startHour =
            ( restriction.start_time.hour, restriction.start_time.minute )

        endHour =
            ( restriction.end_time.hour, restriction.end_time.minute )
    in
    ( Model restriction (initWeekDayList restriction.start_time.weekday) (initHourList startHour "StartHour") (initHourList endHour "EndHour") (initCategoryList restriction.category) data.token data.backendUrl ""
    , Effect.none
    )



-- UPDATE


type Msg
    = SelectWeekday (Select.Msg Time.Weekday)
    | SelectStartHour (Select.Msg ( Int, Int ))
    | SelectEndHour (Select.Msg ( Int, Int ))
    | SelectCategory (Select.Msg Category)
    | UpdateRestrictionRequest
    | UpdateRestrictionResult (Result Http.Error ( RestrictionID, Restriction ))
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl errorMsg) =
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
                            restriction.start_time.weekday

                oldStart_time =
                    restriction.start_time

                oldEnd_time =
                    restriction.end_time
            in
            ( Model { restriction | start_time = { oldStart_time | weekday = newWeekday }, end_time = { oldEnd_time | weekday = newWeekday } } { weekdayList | selectedWeekday = newWeekday, selectState = updatedSelectState } hourStartList hourEndList categoryList token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectWeekday selectCmds) )

        SelectStartHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourStartList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( restriction.start_time.hour, restriction.start_time.minute )

                oldStart_time =
                    restriction.start_time

                newStart_time =
                    { oldStart_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( Model { restriction | start_time = newStart_time } weekdayList { hourStartList | selectedHour = newHour, selectState = updatedSelectState } hourEndList categoryList token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectStartHour selectCmds) )

        SelectEndHour selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg hourEndList.selectState

                newHour =
                    case maybeAction of
                        Just (Select hour) ->
                            hour

                        _ ->
                            ( restriction.end_time.hour, restriction.end_time.minute )

                oldEnd_time =
                    restriction.end_time

                newEnd_time =
                    { oldEnd_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( Model { restriction | end_time = newEnd_time } weekdayList hourStartList { hourEndList | selectedHour = newHour, selectState = updatedSelectState } categoryList token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectEndHour selectCmds) )

        SelectCategory selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg categoryList.selectState

                newCategory =
                    case maybeAction of
                        Just (Select category) ->
                            category

                        _ ->
                            restriction.category
            in
            ( Model { restriction | category = newCategory } weekdayList hourStartList hourEndList { categoryList | selectedCategory = newCategory, selectState = updatedSelectState } token backendUrl errorMsg, Effect.sendCmd (Cmd.map SelectCategory selectCmds) )

        UpdateRestrictionRequest ->
            ( Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl errorMsg, Effect.sendCmd (updateLect restriction backendUrl token) )

        UpdateRestrictionResult result ->
            case result of
                Ok ( updatedID, updatedRestriction ) ->
                    let
                        route =
                            { path = Route.Path.EditLect_Id_ { id = String.fromInt restriction.lect }, query = Dict.empty, hash = Nothing }
                    in
                    ( Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl errorMsg, Effect.updateRestriction ( updatedID, updatedRestriction ) (Just route) )

                Err err ->
                    ( Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl errorMsg, Effect.pushRoute { path = Route.Path.EditLect_Id_ { id = String.fromInt restriction.lect }, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model restriction weekdayList hourStartList hourEndList categoryList token backendUrl errorMsg) =
    { title = "Editar Restrição"
    , body =
        [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect hourEndList "Hora de Fim")
        , Html.map SelectCategory (Html.Styled.toUnstyled <| renderCategorySelect categoryList)
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRestrictionRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }



------------------------ HTTP ------------------------


updateLect : Restriction -> String -> Token -> Cmd Msg
updateLect restriction backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "restrictions"
        , body = Http.jsonBody (Encoders.putRestriction Nothing restriction)
        , expect = Http.expectJson UpdateRestrictionResult (Decoders.responseParser Decoders.getRestrictionAndId)
        , timeout = Nothing
        , tracker = Nothing
        }
