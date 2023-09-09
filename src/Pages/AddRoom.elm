module Pages.AddRoom exposing (Model, Msg, page)

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
import ScheduleObjects.Room exposing (Room, RoomID)
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
    = Model Room String String String


init : Data -> () -> ( Model, Effect Msg )
init shared () =
    ( Model (Room "" "" 0 "") shared.backendUrl shared.token ""
    , Effect.none
    )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | CapacityChange Int
    | NumberChange String
    | UpdateRoomRequest
    | UpdateRoomResult (Result Http.Error RoomID)
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model room backendUrl token errorMsg) =
    case msg of
        AbbrChange str ->
            ( Model { room | abbr = str } backendUrl token errorMsg, Effect.none )

        NameChange str ->
            ( Model { room | name = str } backendUrl token errorMsg, Effect.none )

        CapacityChange int ->
            ( Model { room | capacity = int } backendUrl token errorMsg, Effect.none )

        NumberChange str ->
            ( Model { room | number = str } backendUrl token errorMsg, Effect.none )

        UpdateRoomRequest ->
            ( Model room backendUrl token errorMsg, Effect.sendCmd (updateRoom room backendUrl token) )

        UpdateRoomResult result ->
            case result of
                Ok id ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model room backendUrl token errorMsg, Effect.updateRoom ( id, room ) (Just route) )

                Err err ->
                    ( Model room backendUrl token (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model room backendUrl token errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model room backendUrl token errorMsg) =
    { title = "Criar Sala"
    , body =
        [ input [ class "input-box", style "width" "100%", value room.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value room.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value <| String.fromInt room.capacity, onInput (CapacityChange << Maybe.withDefault room.capacity << String.toInt), Html.Attributes.placeholder "Capacidade" ] []
        , input [ class "input-box", style "width" "100%", value room.number, onInput NumberChange, Html.Attributes.placeholder "Número" ] []
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRoomRequest ] [ text "Submeter" ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }



-- HTTP


updateRoom : Room -> String -> Token -> Cmd Msg
updateRoom room backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms"
        , body = Http.jsonBody (Encoders.putRoom Nothing room)
        , expect = Http.expectJson UpdateRoomResult (JD.field "data" (JD.field "id" JD.int))
        , timeout = Nothing
        , tracker = Nothing
        }
