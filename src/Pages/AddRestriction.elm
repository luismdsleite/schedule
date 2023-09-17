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
import ScheduleObjects.Restriction as Restriction exposing (..)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Select exposing (..)
import SelectLists.Category exposing (CategoryList, initCategoryList, renderCategorySelect, setCategoryList, setCategoryListSelect)
import SelectLists.Hour exposing (HourList, initHourList, renderHourSelect, setEndHourList, setHourListSelect, setStartHourList)
import SelectLists.WeekDay exposing (WeekDayList, initWeekDayList, renderWeekdaySelect, setWeekdayList, setWeekdayListSelect, setWeekdaySelectState)
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


type alias Model =
    { restriction : Restriction
    , weekdayList : WeekDayList
    , hourStartList : HourList
    , hourEndList : HourList
    , categoryList : CategoryList
    , token : Token
    , backendUrl : String
    , errorMsg : String
    }


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
                            model.restriction.start_time.weekday

                oldStart_time =
                    model.restriction.start_time

                oldEnd_time =
                    model.restriction.end_time
            in
            ( model
                |> setRestriction
                    (model.restriction
                        |> setRestStartTime { oldStart_time | weekday = newWeekday }
                        |> setRestEndTime { oldEnd_time | weekday = newWeekday }
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
                            ( model.restriction.start_time.hour, model.restriction.start_time.minute )

                oldStart_time =
                    model.restriction.start_time

                newStart_time =
                    { oldStart_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( model
                |> setRestriction
                    (model.restriction
                        |> setRestStartTime newStart_time
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
                            ( model.restriction.end_time.hour, model.restriction.end_time.minute )

                oldEnd_time =
                    model.restriction.end_time

                newEnd_time =
                    { oldEnd_time | hour = Tuple.first newHour, minute = Tuple.second newHour }
            in
            ( model
                |> setRestriction
                    (model.restriction
                        |> setRestEndTime newEnd_time
                    )
                |> setEndHourList (setWeekdaySelectState updatedSelectState model.hourEndList |> setHourListSelect newHour)
            , Effect.sendCmd (Cmd.map SelectEndHour selectCmds)
            )

        SelectCategory selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.categoryList.selectState

                newCategory =
                    case maybeAction of
                        Just (Select category) ->
                            category

                        _ ->
                            model.restriction.category
            in
            ( model
                |> setRestriction
                    (model.restriction
                        |> setRestCategory newCategory
                    )
                |> setCategoryList (setWeekdaySelectState updatedSelectState model.categoryList |> setCategoryListSelect newCategory)
            , Effect.sendCmd (Cmd.map SelectCategory selectCmds)
            )

        UpdateRestrictionRequest ->
            ( model, Effect.sendCmd (updateRestriction model.restriction model.backendUrl model.token) )

        UpdateRestrictionResult result ->
            case result of
                Ok ( updatedID, updatedRestriction ) ->
                    let
                        route =
                            { path = Route.Path.EditLect_Id_ { id = String.fromInt model.restriction.lect }, query = Dict.empty, hash = Nothing }
                    in
                    ( model, Effect.updateRestriction ( updatedID, updatedRestriction ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.EditLect_Id_ { id = String.fromInt model.restriction.lect }, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Editar Restrição"
    , body =
        [ Html.map SelectWeekday (Html.Styled.toUnstyled <| renderWeekdaySelect model.weekdayList)
        , Html.map SelectStartHour (Html.Styled.toUnstyled <| renderHourSelect model.hourStartList "Hora de Início")
        , Html.map SelectEndHour (Html.Styled.toUnstyled <| renderHourSelect model.hourEndList "Hora de Fim")
        , Html.map SelectCategory (Html.Styled.toUnstyled <| renderCategorySelect model.categoryList)
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRestrictionRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
        ]
    }



------------------------ HTTP ------------------------


updateRestriction : Restriction -> String -> Token -> Cmd Msg
updateRestriction restriction backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "restrictions"
        , body = Http.jsonBody (Encoders.putRestriction Nothing restriction)
        , expect = Http.expectJson UpdateRestrictionResult (Decoders.responseParser Decoders.getRestrictionAndId)
        , timeout = Nothing
        , tracker = Nothing
        }
