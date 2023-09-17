module Pages.AddRoom exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Room exposing (Room, RoomID, asRoomIn, setRoom, setRoomAbbr, setRoomCapacity, setRoomName, setRoomNumber)
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


type alias Model =
    { room : Room
    , backendUrl : String
    , token : String
    , errorMsg : String
    , isHidden : Bool
    }


init : Data -> () -> ( Model, Effect Msg )
init shared () =
    ( Model (Room "" "" 0 "") shared.backendUrl shared.token "" False
    , Effect.none
    )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | CapacityChange Int
    | NumberChange String
    | UpdateRoomRequest
    | UpdateRoomResult (Result Http.Error ( RoomID, ( Room, IsHidden ) ))
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    -- update msg (Model room backendUrl token errorMsg isHidden) =
    case msg of
        AbbrChange str ->
            ( setRoomAbbr str model.room |> asRoomIn model, Effect.none )

        NameChange str ->
            ( setRoomName str model.room |> asRoomIn model, Effect.none )

        CapacityChange int ->
            ( setRoomCapacity int model.room |> asRoomIn model, Effect.none )

        NumberChange str ->
            ( setRoomNumber str model.room |> asRoomIn model, Effect.none )

        UpdateRoomRequest ->
            ( model, Effect.sendCmd (updateRoom model.room model.isHidden model.backendUrl model.token) )

        UpdateRoomResult result ->
            case result of
                Ok ( id, newRoom ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( model, Effect.updateRoom ( id, newRoom ) (Just route) )

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
    { title = "Criar Sala"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.room.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.room.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value <| String.fromInt model.room.capacity, onInput (CapacityChange << Maybe.Extra.withDefaultLazy (\() -> model.room.capacity) << String.toInt), Html.Attributes.placeholder "Capacidade" ] []
        , input [ class "input-box", style "width" "100%", value model.room.number, onInput NumberChange, Html.Attributes.placeholder "Número" ] []
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Sala" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRoomRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
        ]
    }



-- HTTP


updateRoom : Room -> IsHidden -> String -> Token -> Cmd Msg
updateRoom room isHidden backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms"
        , body = Http.jsonBody (Encoders.putRoom Nothing room isHidden)
        , expect = Http.expectJson UpdateRoomResult (Decoders.responseParser Decoders.getRoomAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
