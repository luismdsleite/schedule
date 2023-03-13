module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)
import ScheduleObjects.Data exposing (Data)
import Dict
import Gen.Route


type alias Flags =
    Json.Value


type alias Model =
    {data : Data}


type Msg
    = LoadedData Data


init : Request -> Flags -> ( Model, Cmd Msg )
init req _ =
    ( {data = (Data Dict.empty Dict.empty Dict.empty Dict.empty)}, Request.pushRoute Gen.Route.Home_ req )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        LoadedData data ->
            ( {model | data = data}, Request.pushRoute Gen.Route.Main req )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
