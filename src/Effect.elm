module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , map, toCmd
    , deleteBlock, deleteEvent, deleteLect, deleteOccupation, deleteRestriction, deleteRoom, loadData, loadToken, updateBlock, updateEvent, updateLect, updateOccupation, updateRestriction, updateRoom
    )

{-|

@docs Effect
@docs none, batch
@docs sendCmd, sendMsg
@docs pushRoute, replaceRoute, loadExternalUrl

@docs map, toCmd

-}

import Browser.Navigation
import Dict exposing (Dict)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Restriction exposing (Restriction, RestrictionID)
import ScheduleObjects.Room exposing (Room, RoomID)
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Redirect users to a new URL, somewhere external your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg


loadToken : String -> Effect msg
loadToken token =
    SendSharedMsg (Shared.Msg.GotToken token)


loadData : Data -> Effect msg
loadData data =
    SendSharedMsg (Shared.Msg.GotData data)


{-| Update an event in the shared model. Optionally you can also give a route to go into (Effect.pushRoute) after the event has been updated.
-}
updateEvent :
    ( EventID, ( Event, IsHidden ) )
    ->
        Maybe
            { path : Route.Path.Path
            , query : Dict String String
            , hash : Maybe String
            }
    -> Effect msg
updateEvent ( evID, ev ) maybeRoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateEvent ( evID, ev )) maybeRoute)


updateRoom : ( RoomID, ( Room, IsHidden ) ) -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
updateRoom ( roomID, room ) maybeRoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateRoom ( roomID, room )) maybeRoute)


updateLect : ( LecturerID, ( Lecturer, IsHidden ) ) -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
updateLect ( lectID, lect ) mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateLect ( lectID, lect )) mayberoute)


updateBlock : ( BlockID, ( Block, IsHidden ) ) -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
updateBlock ( blockID, block ) mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateBlock ( blockID, block )) mayberoute)


updateRestriction : ( RestrictionID, Restriction ) -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
updateRestriction ( restrictionID, restriction ) mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateRestriction ( restrictionID, restriction )) mayberoute)


updateOccupation : ( OccupationID, Occupation ) -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
updateOccupation ( occupationID, occupation ) mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.UpdateOccupation ( occupationID, occupation )) mayberoute)


deleteEvent : EventID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteEvent evID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteEvent evID) mayberoute)


deleteRoom : RoomID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteRoom roomID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteRoom roomID) mayberoute)


deleteLect : LecturerID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteLect lectID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteLect lectID) mayberoute)


deleteBlock : BlockID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteBlock blockID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteBlock blockID) mayberoute)


deleteRestriction : RestrictionID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteRestriction restrictionID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteRestriction restrictionID) mayberoute)


deleteOccupation : OccupationID -> Maybe { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
deleteOccupation occupationID mayberoute =
    SendSharedMsg (Shared.Msg.UpdateData (Shared.Msg.DeleteOccupation occupationID) mayberoute)
