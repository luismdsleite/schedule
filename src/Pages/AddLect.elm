module Pages.AddLect exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Model Lecturer String String String


init : Data -> () -> ( Model, Effect Msg )
init shared () =
    ( Model (Lecturer "" "" "") shared.backendUrl shared.token ""
    , Effect.none
    )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | OfficeChange String
    | UpdateLectRequest
    | UpdateLectResult (Result Http.Error LecturerID)
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model lect backendUrl token errorMsg) =
    case msg of
        AbbrChange str ->
            ( Model { lect | abbr = str } backendUrl token errorMsg, Effect.none )

        NameChange str ->
            ( Model { lect | name = str } backendUrl token errorMsg, Effect.none )

        OfficeChange str ->
            ( Model { lect | office = str } backendUrl token errorMsg, Effect.none )

        UpdateLectRequest ->
            ( Model lect backendUrl token errorMsg, Effect.sendCmd (updateLect lect backendUrl token) )

        UpdateLectResult result ->
            case result of
                Ok id ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model lect backendUrl token errorMsg, Effect.updateLect ( id, lect ) (Just route) )

                Err err ->
                    ( Model lect backendUrl token (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model lect backendUrl token errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model lect backendUrl token errorMsg) =
    { title = "Criar Docente"
    , body =
        [ input [ class "input-box", style "width" "100%", value lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value lect.office, onInput OfficeChange, Html.Attributes.placeholder "Número" ] []
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }



-- HTTP


updateLect : Lecturer -> String -> Token -> Cmd Msg
updateLect lect backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers"
        , body = Http.jsonBody (Encoders.putLecturer Nothing lect)
        , expect = Http.expectJson UpdateLectResult (JD.field "data" (JD.field "id" JD.int))
        , timeout = Nothing
        , tracker = Nothing
        }
