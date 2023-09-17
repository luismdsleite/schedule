module Pages.EditRestriction.Id_ exposing (Model, Msg, page)

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
import ScheduleObjects.Restriction as Restriction exposing (Category, Restriction, RestrictionID, setRestCategory, setRestEndTime, setRestStartTime, setRestriction)
import ScheduleObjects.WeekTime exposing (WeekTime)
import Select exposing (..)
import SelectLists.Category exposing (CategoryList, initCategoryList, renderCategorySelect, setCategoryList, setCategoryListSelect)
import SelectLists.Hour exposing (HourList, initHourList, renderHourSelect, setEndHourList, setHourListSelect, setStartHourList)
import SelectLists.WeekDay exposing (WeekDayList, initWeekDayList, renderWeekdaySelect, setWeekdayList, setWeekdayListSelect, setWeekdaySelectState)
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
    { restrictionID : RestrictionID
    , restriction : Restriction
    , weekdayList : WeekDayList
    , hourStartList : HourList
    , hourEndList : HourList
    , categoryList : CategoryList
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

        restriction =
            Dict.get id data.restrictions |> Maybe.Extra.withDefaultLazy (\() -> Restriction -1 (WeekTime Time.Mon 9 0) (WeekTime Time.Mon 10 0) Restriction.Preference)

        startHour =
            ( restriction.start_time.hour, restriction.start_time.minute )

        endHour =
            ( restriction.end_time.hour, restriction.end_time.minute )
    in
    ( Model id restriction (initWeekDayList restriction.start_time.weekday) (initHourList startHour "StartHour") (initHourList endHour "EndHour") (initCategoryList restriction.category) data.token data.backendUrl "" False
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
    | DeleteRestrictionRequest
    | DeleteRestrictionResult (Result Http.Error ())
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
            ( model, Effect.sendCmd (updateRestriction ( model.restrictionID, model.restriction ) model.backendUrl model.token) )

        DeleteRestrictionRequest ->
            if model.deleteConfirmation then
                ( { model | deleteConfirmation = False }, Effect.sendCmd (deleteRestriction model.restrictionID model.backendUrl model.token) )

            else
                ( { model | deleteConfirmation = True }, Effect.none )

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

        DeleteRestrictionResult result ->
            case result of
                Ok _ ->
                    let
                        route =
                            { path = Route.Path.EditLect_Id_ { id = String.fromInt model.restriction.lect }, query = Dict.empty, hash = Nothing }
                    in
                    ( model, Effect.deleteRestriction model.restrictionID (Just route) )

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
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteRestrictionRequest ]
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


updateRestriction : ( RestrictionID, Restriction ) -> String -> Token -> Cmd Msg
updateRestriction ( id, restriction ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "restrictions\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putRestriction Nothing restriction)
        , expect = Http.expectJson UpdateRestrictionResult (Decoders.responseParser Decoders.getRestrictionAndId)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteRestriction : RestrictionID -> String -> Token -> Cmd Msg
deleteRestriction id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "restrictions\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteRestrictionResult (Ok ())

        Err err ->
            DeleteRestrictionResult (Err err)
