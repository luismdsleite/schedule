module Pages.AddLect exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID, asLectIn, setLect, setLectAbbr, setLectName, setLectOffice)
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


type alias Model =
    { lect : Lecturer, backendUrl : String, token : String, errorMsg : String, isHidden : Bool }


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
update msg model =
    case msg of
        AbbrChange str ->
            ( setLectAbbr str model.lect |> asLectIn model, Effect.none )

        NameChange str ->
            ( setLectName str model.lect |> asLectIn model, Effect.none )

        OfficeChange str ->
            ( setLectOffice str model.lect |> asLectIn model, Effect.none )

        UpdateLectRequest ->
            ( model, Effect.sendCmd (updateLect model.lect model.isHidden model.backendUrl model.token) )

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
                    ( model, Effect.updateLect ( id, updatedLect ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        VisibilityChange newVisibility ->
            ( { model | isHidden = newVisibility }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Criar Docente"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value model.lect.office, onInput OfficeChange, Html.Attributes.placeholder "Número" ] []
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Docente" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
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
