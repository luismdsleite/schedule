module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Dict
import ElmSpa.Request exposing (Request)
import Gen.Route
import Json.Decode as Json
import Request exposing (Request)
import ScheduleObjects.Data exposing (Data)


type alias Flags =
    Json.Value


type alias Model =
    Data


type Msg
    = LoadedData Data


init : Request -> Flags -> ( Model, Cmd Msg )
init req _ =
    ( Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty, Request.pushRoute Gen.Route.Home_ req )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        LoadedData data ->
            ( data, Request.pushRoute Gen.Route.Main req )



-- UpdateEventResult response ->
--     case response of
--         Ok ( id, updatedEvent ) ->
--             let
--                 newEvents =
--                     model.events |> Dict.insert id updatedEvent
--             in
--             ( { model | events = newEvents }, Cmd.none )
--         Err ( id, unmodifiedEvent ) ->
--             let
--                 newEvents =
--                     model.events |> Dict.insert id unmodifiedEvent
--             in
--             ( { model | events = newEvents }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
