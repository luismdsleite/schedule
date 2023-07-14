module Pages.Home_ exposing (Model, Msg, page)

{-|

1.  Loads data via GET requests. Fetches events, rooms, lecturers, block resources.
2.  Updates to `Shared.Model` via `Shared.LoadedData` msg
3.  Redirect to Pages.Example

-}

import Array exposing (Array)
import Decoders exposing (eventParser, lectParser, objectsToDictParser, roomParser)
import DeployEnv exposing (serverUrl)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
import Html
import Http
import Json.Decode exposing (Decoder)
import Page
import Request
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Room exposing (Room)
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| This page has 3 possible states:

    - Loading: We are still waiting for 1 or more GET request. To know if a specific resource was already received, we store a `Array Bool` that represents the state of each resource.
    - Failed: One of the requests failed.
    - Loaded: We received all request and can start the `Shared.LoadedData` msg.

-}
type Model
    = Loading Data (Array Bool)
    | Loaded Data
    | Failed String



-- INIT


init : ( Model, Effect Msg )
init =
    let
        emptyData =
            Data Dict.empty Dict.empty Dict.empty Dict.empty

        noneReceived =
            Array.fromList [ False, False, False ]

        getRequests =
            [ getEvents, getLecturers, getRooms ]
    in
    ( Loading emptyData noneReceived, Effect.batch getRequests )



-- UPDATE


type Msg
    = GotRooms (Result Http.Error (Dict ID Room))
    | GotLecturers (Result Http.Error (Dict ID Lecturer))
    | GotEvents (Result Http.Error (Dict ID Event))
    | LoadedData Data


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotRooms result ->
            case result of
                Err _ ->
                    ( Failed "Unable to load Rooms", Effect.none )

                Ok rooms ->
                    case model of
                        Loading data state ->
                            let
                                updatedData =
                                    { data | rooms = rooms }
                            in
                            if Array.foldl (&&) True (Array.set 0 True state) then
                                update (LoadedData data) (Loaded updatedData)

                            else
                                ( Loading updatedData (Array.set 0 True state), Effect.none )

                        _ ->
                            ( Failed "Unable to load Rooms", Effect.none )

        GotEvents result ->
            case result of
                Err _ ->
                    ( Failed "Unable to load Events", Effect.none )

                Ok events ->
                    case model of
                        Loading data state ->
                            let
                                updatedData =
                                    { data | events = events }
                            in
                            if Array.foldl (&&) True (Array.set 1 True state) then
                                update (LoadedData updatedData) (Loaded updatedData)

                            else
                                ( Loading updatedData (Array.set 1 True state), Effect.none )

                        _ ->
                            ( Failed "Unable to load Events", Effect.none )

        GotLecturers result ->
            case result of
                Err _ ->
                    ( Failed "Unable to load Lecturers", Effect.none )

                Ok lecturers ->
                    case model of
                        Loading data state ->
                            let
                                updatedData =
                                    { data | lecturers = lecturers }
                            in
                            if Array.foldl (&&) True (Array.set 2 True state) then
                                update (LoadedData updatedData) (Loaded updatedData)

                            else
                                ( Loading updatedData (Array.set 2 True state), Effect.none )

                        _ ->
                            ( Failed "Unable to load Lecturers", Effect.none )

        LoadedData data ->
            ( model, Effect.fromShared (Shared.LoadedData data) )


getResource : String -> Decoder a -> (Result Http.Error (Dict ID a) -> msg) -> Cmd msg
getResource resource resourceParser resultToMsg =
    Http.get
        { url = serverUrl ++ resource
        , expect = Http.expectJson resultToMsg (objectsToDictParser resourceParser)
        }


getRooms : Effect Msg
getRooms =
    Effect.fromCmd (getResource "rooms" roomParser GotRooms)


getLecturers : Effect Msg
getLecturers =
    Effect.fromCmd (getResource "lecturers" lectParser GotLecturers)


getEvents : Effect Msg
getEvents =
    Effect.fromCmd (getResource "events" eventParser GotEvents)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    case model of
        Loading _ _ ->
            generateHtml "Loading" "Loading"

        Failed str ->
            generateHtml "Failed" str

        Loaded _ ->
            generateHtml "Loaded" "Loaded"


generateHtml : String -> String -> View Msg
generateHtml title body =
    { title = title
    , body = [ Html.text body ]
    }
