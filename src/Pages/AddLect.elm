module Pages.AddLect exposing (Model, Msg, page)

import Decoders exposing (IsHidden)
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
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
    = Model Lecturer String String String Bool


init : Data -> () -> ( Model, Effect Msg )
init shared () =
    ( Model (Lecturer "" "" "") shared.backendUrl shared.token "" False
    , Effect.none
    )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | OfficeChange String
    | UpdateLectRequest
    | UpdateLectResult (Result Http.Error ( LecturerID, ( Lecturer, IsHidden ) ))
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model lect backendUrl token errorMsg isHidden) =
    case msg of
        AbbrChange str ->
            ( Model { lect | abbr = str } backendUrl token errorMsg isHidden, Effect.none )

        NameChange str ->
            ( Model { lect | name = str } backendUrl token errorMsg isHidden, Effect.none )

        OfficeChange str ->
            ( Model { lect | office = str } backendUrl token errorMsg isHidden, Effect.none )

        UpdateLectRequest ->
            ( Model lect backendUrl token errorMsg isHidden, Effect.sendCmd (updateLect lect isHidden backendUrl token) )

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
                    ( Model lect backendUrl token errorMsg isHidden, Effect.updateLect ( id, updatedLect ) (Just route) )

                Err err ->
                    ( Model lect backendUrl token (Decoders.errorToString err) isHidden, Effect.none )

        VisibilityChange newVisibility ->
            ( Model lect backendUrl token errorMsg newVisibility, Effect.none )

        Return ->
            ( Model lect backendUrl token errorMsg isHidden, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model lect backendUrl token errorMsg isHidden) =
    { title = "Criar Docente"
    , body =
        [ input [ class "input-box", style "width" "100%", value lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value lect.office, onInput OfficeChange, Html.Attributes.placeholder "Número" ] []
        , div [] [ input [ type_ "checkbox", checked isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Docente" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }



-- HTTP


updateLect : Lecturer -> IsHidden -> String -> Token -> Cmd Msg
updateLect lect isHidden backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers"
        , body = Http.jsonBody (Encoders.putLecturer Nothing lect isHidden)
        , expect = Http.expectJson UpdateLectResult (Decoders.responseParser Decoders.getLectAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
