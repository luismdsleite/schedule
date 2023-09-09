module Pages.EditRoom.Id_ exposing (Model, Msg, page)

import Decoders
import Dict
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Room exposing (Room, RoomID)
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
    = Model ( RoomID, Room ) String String Bool String


init : Data -> String -> () -> ( Model, Effect Msg )
init data roomIDParam () =
    let
        roomID =
            String.toInt roomIDParam
                |> Maybe.withDefault -1

        room =
            Dict.get roomID data.rooms |> Maybe.withDefault (Room "" "" 0 "")
    in
    ( Model ( roomID, room ) data.backendUrl data.token False "", Effect.none )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | CapacityChange Int
    | NumberChange String
    | UpdateRoomRequest
    | UpdateRoomResult (Result Http.Error ( RoomID, Room ))
    | DeleteRoomRequest
    | DeleteRoomResult (Result Http.Error ())
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model ( roomID, room ) backendUrl token deleteConfirmation errorMsg) =
    case msg of
        AbbrChange str ->
            ( Model ( roomID, { room | abbr = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        NameChange str ->
            ( Model ( roomID, { room | name = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        CapacityChange int ->
            ( Model ( roomID, { room | capacity = int } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        NumberChange str ->
            ( Model ( roomID, { room | number = str } ) backendUrl token deleteConfirmation errorMsg, Effect.none )

        UpdateRoomRequest ->
            ( Model ( roomID, room ) backendUrl token deleteConfirmation errorMsg, Effect.sendCmd <| updateRoom ( roomID, room ) backendUrl token )

        UpdateRoomResult result ->
            case result of
                Ok ( id, updtedRoom ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model ( id, updtedRoom ) backendUrl token deleteConfirmation errorMsg, Effect.updateRoom ( id, updtedRoom ) (Just route) )

                Err err ->
                    ( Model ( roomID, room ) backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        DeleteRoomRequest ->
            if deleteConfirmation then
                ( Model ( roomID, room ) backendUrl token False errorMsg, Effect.sendCmd <| deleteRoom roomID backendUrl token )

            else
                ( Model ( roomID, room ) backendUrl token True errorMsg, Effect.none )

        DeleteRoomResult result ->
            case result of
                Ok _ ->
                    ( Model ( roomID, room ) backendUrl token deleteConfirmation errorMsg, Effect.deleteRoom roomID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( Model ( roomID, room ) backendUrl token deleteConfirmation "Erro no servidor. Verifique se ainda existem eventos (incluindo os eventos escondidos) associados a esta sala.", Effect.none )

                Err err ->
                    ( Model ( roomID, room ) backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        Return ->
            ( Model ( roomID, room ) backendUrl token deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model ( roomID, room ) backendUrl token deleteConfirmation errorMsg) =
    { title = "Editar Sala"
    , body =
        [ input [ class "input-box", style "width" "100%", value room.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value room.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value <| String.fromInt room.capacity, onInput (CapacityChange << Maybe.withDefault room.capacity << String.toInt), Html.Attributes.placeholder "Capacidade" ] []
        , input [ class "input-box", style "width" "100%", value room.number, onInput NumberChange, Html.Attributes.placeholder "Número" ] []
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRoomRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteRoomRequest ]
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


updateRoom : ( RoomID, Room ) -> String -> Token -> Cmd Msg
updateRoom ( id, room ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putRoom Nothing room)
        , expect = Http.expectWhatever (handleUpdateResponse ( id, room ))
        , timeout = Nothing
        , tracker = Nothing
        }


handleUpdateResponse : ( RoomID, Room ) -> Result Http.Error () -> Msg
handleUpdateResponse ( roomID, room ) response =
    case response of
        Ok _ ->
            UpdateRoomResult (Ok ( roomID, room ))

        Err err ->
            UpdateRoomResult (Err err)


deleteRoom : RoomID -> String -> Token -> Cmd Msg
deleteRoom id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteRoomResult (Ok ())

        Err err ->
            DeleteRoomResult (Err err)
