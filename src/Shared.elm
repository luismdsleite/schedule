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

import Dict exposing (Dict)
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Id exposing (ID)
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
            ( Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty "" flags.server_url
            , Effect.pushRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }
            )

        Err _ ->
            ( Data Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty "" ""
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
                Shared.Msg.UpdateEvent ( evID, ( ev, isHidden ) ) ->
                    let
                        cleansedModel =
                            { model | events = Dict.remove evID model.events, hiddenEvents = Dict.remove evID model.hiddenEvents }

                        newModel =
                            if isHidden then
                                { cleansedModel | hiddenEvents = Dict.insert evID ev model.hiddenEvents }

                            else
                                { cleansedModel | events = Dict.insert evID ev model.events }
                    in
                    ( newModel, effect )

                Shared.Msg.UpdateRoom ( roomID, ( room, isHidden ) ) ->
                    let
                        cleansedModel =
                            { model | rooms = Dict.remove roomID model.rooms, hiddenRooms = Dict.remove roomID model.hiddenRooms }

                        newModel =
                            if isHidden then
                                { cleansedModel | hiddenRooms = Dict.insert roomID room model.hiddenRooms }

                            else
                                { cleansedModel | rooms = Dict.insert roomID room model.rooms }
                    in
                    ( newModel, effect )

                Shared.Msg.UpdateLect ( lectID, ( lect, isHidden ) ) ->
                    let
                        cleansedModel =
                            { model | lecturers = Dict.remove lectID model.lecturers, hiddenLecturers = Dict.remove lectID model.hiddenLecturers }

                        newModel =
                            if isHidden then
                                { cleansedModel | hiddenLecturers = Dict.insert lectID lect model.hiddenLecturers }

                            else
                                { cleansedModel | lecturers = Dict.insert lectID lect model.lecturers }
                    in
                    ( newModel, effect )

                Shared.Msg.UpdateBlock ( blockID, ( block, isHidden ) ) ->
                    let
                        cleansedModel =
                            { model | blocks = Dict.remove blockID model.blocks, hiddenBlocks = Dict.remove blockID model.hiddenBlocks }

                        newModel =
                            if isHidden then
                                { cleansedModel | hiddenBlocks = Dict.insert blockID block model.hiddenBlocks }

                            else
                                { cleansedModel | blocks = Dict.insert blockID block model.blocks }
                    in
                    ( newModel, effect )

                Shared.Msg.UpdateRestriction ( restID, rest ) ->
                    ( { model | restrictions = Dict.insert restID rest model.restrictions }
                    , effect
                    )

                Shared.Msg.UpdateOccupation ( occID, occ ) ->
                    ( { model | occupations = Dict.insert occID occ model.occupations }
                    , effect
                    )

                Shared.Msg.DeleteEvent evID ->
                    ( { model | events = Dict.remove evID model.events, hiddenEvents = Dict.remove evID model.hiddenEvents }
                    , effect
                    )

                Shared.Msg.DeleteRoom roomID ->
                    ( { model | rooms = Dict.remove roomID model.rooms, hiddenRooms = Dict.remove roomID model.hiddenRooms }
                    , effect
                    )

                Shared.Msg.DeleteLect lectID ->
                    ( { model | lecturers = Dict.remove lectID model.lecturers, hiddenLecturers = Dict.remove lectID model.hiddenLecturers }
                    , effect
                    )

                Shared.Msg.DeleteBlock blockID ->
                    ( { model | blocks = Dict.remove blockID model.blocks, hiddenBlocks = Dict.remove blockID model.hiddenBlocks }
                    , effect
                    )

                Shared.Msg.DeleteRestriction restID ->
                    ( { model | restrictions = Dict.remove restID model.restrictions }
                    , effect
                    )

                Shared.Msg.DeleteOccupation occID ->
                    ( { model | occupations = Dict.remove occID model.occupations }
                    , effect
                    )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
