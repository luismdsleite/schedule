module Pages.Load exposing (Model, Msg, page)

{-|

1.  Loads data via GET requests. Fetches events, rooms, lecturers, block resources.
2.  Updates to `Shared.Model` via `Shared.LoadedData` msg
3.  Redirect to Pages.Example

-}

import Array exposing (Array)
import Decoders exposing (blockParser, eventParser, lectParser, objectsToDictParser, occupationParser, restrictionParser, roomParser)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
import Http
import Json.Decode exposing (Decoder)
import Page exposing (Page)
import Route exposing (Route)
import ScheduleObjects.Block exposing (Block)
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (Lecturer)
import ScheduleObjects.Occupation exposing (Occupation)
import ScheduleObjects.Restriction exposing (Restriction)
import ScheduleObjects.Room exposing (Room)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared.backendUrl shared.token
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
    = Loading Data GotData
    | Loaded Data
    | Failed String


type alias GotData =
    { gotRooms : Bool
    , gotLecturers : Bool
    , gotEvents : Bool
    , gotBlocks : Bool
    , gotOccupations : Bool
    , gotRestrictions : Bool
    }


receivedAllData : GotData -> Bool
receivedAllData gotData =
    gotData.gotRooms && gotData.gotLecturers && gotData.gotEvents && gotData.gotBlocks && gotData.gotOccupations && gotData.gotRestrictions


handleValidHttpResult : GotData -> Data -> ( Model, Effect Msg )
handleValidHttpResult newState updatedData =
    if receivedAllData newState then
        update (LoadedData updatedData) (Loaded updatedData)

    else
        ( Loading updatedData newState, Effect.none )



-- INIT


init : String -> Token -> () -> ( Model, Effect Msg )
init backendUrl token () =
    let
        emptyData =
            Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty token backendUrl

        noneReceived =
            GotData False False False False False False

        getRequests =
            List.map (\req -> req backendUrl)
                [ getEvents, getLecturers, getRooms, getBlocks, getOccupations, getRestrictions ]
                |> List.map (\req -> req token)
    in
    ( Loading emptyData noneReceived, Effect.batch getRequests )



-- UPDATE


type Msg
    = GotRooms (Result Http.Error (Dict ID ( Room, IsHidden )))
    | GotLecturers (Result Http.Error (Dict ID ( Lecturer, IsHidden )))
    | GotEvents (Result Http.Error (Dict ID ( Event, IsHidden )))
    | GotBlocks (Result Http.Error (Dict ID ( Block, IsHidden )))
    | GotOccupations (Result Http.Error (Dict ID Occupation))
    | GotRestrictions (Result Http.Error (Dict ID Restriction))
    | LoadedData Data


hidden : a -> ( b, IsHidden ) -> Bool
hidden _ ( _, bool ) =
    bool


notHidden : a -> ( b, IsHidden ) -> Bool
notHidden _ ( _, bool ) =
    not bool


removeHiddenMap : Dict a ( b, IsHidden ) -> Dict a b
removeHiddenMap =
    Dict.map (\_ ( b, _ ) -> b)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotRooms result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok rooms ->
                    case model of
                        Loading data state ->
                            let
                                exposedRooms =
                                    Dict.filter notHidden rooms
                                        |> removeHiddenMap

                                hiddenRooms =
                                    Dict.filter hidden rooms
                                        |> removeHiddenMap

                                updatedData =
                                    { data | rooms = exposedRooms, hiddenRooms = hiddenRooms }

                                newState =
                                    { state | gotRooms = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        GotEvents result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok events ->
                    case model of
                        Loading data state ->
                            let
                                exposedEvents =
                                    Dict.filter notHidden events
                                        |> removeHiddenMap

                                hiddenEvents =
                                    Dict.filter hidden events
                                        |> removeHiddenMap

                                updatedData =
                                    { data | events = exposedEvents, hiddenEvents = hiddenEvents }

                                newState =
                                    { state | gotEvents = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        GotLecturers result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok lecturers ->
                    case model of
                        Loading data state ->
                            let
                                exposedLecturers =
                                    Dict.filter notHidden lecturers
                                        |> removeHiddenMap

                                hiddenLecturers =
                                    Dict.filter hidden lecturers
                                        |> removeHiddenMap

                                updatedData =
                                    { data | lecturers = exposedLecturers, hiddenLecturers = hiddenLecturers }

                                newState =
                                    { state | gotLecturers = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        GotBlocks result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok blocks ->
                    case model of
                        Loading data state ->
                            let
                                exposedBlocks =
                                    Dict.filter notHidden blocks
                                        |> removeHiddenMap

                                hiddenBlocks =
                                    Dict.filter hidden blocks
                                        |> removeHiddenMap

                                updatedData =
                                    { data | blocks = exposedBlocks, hiddenBlocks = hiddenBlocks }

                                newState =
                                    { state | gotBlocks = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        GotOccupations result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok occupations ->
                    case model of
                        Loading data state ->
                            let
                                updatedData =
                                    { data | occupations = occupations }

                                newState =
                                    { state | gotOccupations = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        GotRestrictions result ->
            case result of
                Err err ->
                    ( Failed (Decoders.errorToString err), Effect.none )

                Ok restrictions ->
                    case model of
                        Loading data state ->
                            let
                                updatedData =
                                    { data | restrictions = restrictions }

                                newState =
                                    { state | gotRestrictions = True }
                            in
                            handleValidHttpResult newState updatedData

                        state ->
                            ( state, Effect.none )

        LoadedData data ->
            ( model, Effect.loadData data )


getResource : String -> Decoder a -> (Result Http.Error (Dict ID a) -> msg) -> String -> Token -> Cmd msg
getResource resource resourceParser resultToMsg backendUrl token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = backendUrl ++ resource
        , body = Http.emptyBody
        , expect = Http.expectJson resultToMsg (objectsToDictParser resourceParser)
        , timeout = Nothing
        , tracker = Nothing
        }


getRooms : String -> Token -> Effect Msg
getRooms backendUrl token =
    Effect.sendCmd (getResource "rooms" roomParser GotRooms backendUrl token)


getLecturers : String -> Token -> Effect Msg
getLecturers backendUrl token =
    Effect.sendCmd (getResource "lecturers" lectParser GotLecturers backendUrl token)


getEvents : String -> Token -> Effect Msg
getEvents backendUrl token =
    Effect.sendCmd (getResource "events" eventParser GotEvents backendUrl token)


getBlocks : String -> Token -> Effect Msg
getBlocks backendUrl token =
    Effect.sendCmd (getResource "blocks" blockParser GotBlocks backendUrl token)


getOccupations : String -> Token -> Effect Msg
getOccupations backendUrl token =
    Effect.sendCmd (getResource "occupations" occupationParser GotOccupations backendUrl token)


getRestrictions : String -> Token -> Effect Msg
getRestrictions backendUrl token =
    Effect.sendCmd (getResource "restrictions" restrictionParser GotRestrictions backendUrl token)



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
