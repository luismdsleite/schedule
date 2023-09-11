module Pages.EditLect.Id_ exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import Shared
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
    = Model ( LecturerID, Lecturer ) String String Bool String


init : Data -> String -> () -> ( Model, Effect Msg )
init data lectIDParam () =
    let
        lectID =
            String.toInt lectIDParam
                |> Maybe.Extra.withDefaultLazy (\() -> -1)

        lect =
            Dict.get lectID data.lecturers |> Maybe.Extra.withDefaultLazy (\() -> Lecturer "" "" "")
    in
    ( Model ( lectID, lect ) data.backendUrl data.token False "", Effect.none )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | OfficeChange String
    | UpdateLectRequest
    | UpdateLectResult (Result Http.Error ( LecturerID, Lecturer ))
    | DeleteLectRequest
    | DeleteLectResult (Result Http.Error ())
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model ( lectID, lect ) backendUrl token deleteConfirmation errorMsg) =
    case msg of
        AbbrChange str ->
            ( Model ( lectID, { lect | abbr = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        NameChange str ->
            ( Model ( lectID, { lect | name = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        OfficeChange str ->
            ( Model ( lectID, { lect | office = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        UpdateLectRequest ->
            ( Model ( lectID, lect ) backendUrl token deleteConfirmation errorMsg, Effect.sendCmd <| updateLect ( lectID, lect ) backendUrl token )

        UpdateLectResult result ->
            case result of
                Ok ( id, updatedLect ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model ( id, updatedLect ) backendUrl token deleteConfirmation errorMsg, Effect.updateLect ( id, updatedLect ) (Just route) )

                Err err ->
                    ( Model ( lectID, lect ) backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        DeleteLectRequest ->
            if deleteConfirmation then
                ( Model ( lectID, lect ) backendUrl token False errorMsg, Effect.sendCmd <| deleteLect lectID backendUrl token )

            else
                ( Model ( lectID, lect ) backendUrl token True errorMsg, Effect.none )

        DeleteLectResult result ->
            case result of
                Ok _ ->
                    ( Model ( lectID, lect ) backendUrl token deleteConfirmation errorMsg, Effect.deleteLect lectID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( Model ( lectID, lect ) backendUrl token deleteConfirmation "Erro no servidor. Verifique se ainda existem cadeiras (incluindo cadeiras escondidas) associadas a este docente.", Effect.none )

                Err err ->
                    ( Model ( lectID, lect ) backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model ( lectID, lect ) backendUrl token deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model ( lectID, lect ) backendUrl token deleteConfirmation errorMsg) =
    { title = "Editar Docente"
    , body =
        [ input [ class "input-box", style "width" "100%", value lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Do Docente" ] []
        , input [ class "input-box", style "width" "100%", value lect.office, onInput OfficeChange, Html.Attributes.placeholder "Escritorio" ] []
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteLectRequest ]
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


updateLect : ( LecturerID, Lecturer ) -> String -> Token -> Cmd Msg
updateLect ( id, lect ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putLecturer Nothing lect)
        , expect = Http.expectJson UpdateLectResult (Decoders.responseParser Decoders.getLectAndID)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteLect : LecturerID -> String -> Token -> Cmd Msg
deleteLect id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteLectResult (Ok ())

        Err err ->
            DeleteLectResult (Err err)
