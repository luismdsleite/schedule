module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Dict
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data)
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { server_url : String }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field "server_url" Json.Decode.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    case flagsResult of
        Ok flags ->
            ( Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty "" flags.server_url
            , Effect.pushRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }
            )

        Err _ ->
            ( Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty "" ""
            , Effect.pushRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }
            )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.GotToken token ->
            ( { model | token = token }
            , Effect.pushRoute { path = Route.Path.Load, query = Dict.empty, hash = Nothing }
            )

        Shared.Msg.GotData data ->
            ( data
            , Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing }
            )

        Shared.Msg.UpdateData req maybeRoute ->
            let
                effect =
                    case maybeRoute of
                        Just r ->
                            Effect.pushRoute r

                        Nothing ->
                            Effect.none
            in
            case req of
                Shared.Msg.UpdateEvent ( evID, ev ) ->
                    ( { model | events = Dict.insert evID ev model.events }
                    , effect
                      -- , Effect.pushRoute { path = route.path, query = Dict.insert "teste1" "teste" Dict.empty, hash = Nothing }
                    )

                Shared.Msg.DeleteEvent evID ->
                    ( { model | events = Dict.remove evID model.events }
                    , effect
                    )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
