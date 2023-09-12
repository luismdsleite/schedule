module Pages.EditRoom.Id_ exposing (Model, Msg, page)

import Decoders
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick, onInput)
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (convertWeekDay, convertWeekTimeHourAndMinute, weekTimeComparator)
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
    = Model ( RoomID, Room ) (Dict OccupationID Occupation) String String Bool String


init : Data -> String -> () -> ( Model, Effect Msg )
init data roomIDParam () =
    let
        roomID =
            String.toInt roomIDParam
                |> Maybe.Extra.withDefaultLazy (\_ -> -1)

        room =
            Dict.get roomID data.rooms |> Maybe.Extra.withDefaultLazy (\() -> Room "" "" 0 "")
    in
    ( Model ( roomID, room ) (Dict.filter (\_ occupations -> occupations.room == roomID) data.occupations) data.backendUrl data.token False "", Effect.none )



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
    | GoToEditOccupationPage OccupationID
    | GoToAddOccupationPage
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg) =
    case msg of
        AbbrChange str ->
            ( Model ( roomID, { room | abbr = str } ) occupations backendUrl token deleteConfirmation errorMsg, Effect.none )

        NameChange str ->
            ( Model ( roomID, { room | name = str } ) occupations backendUrl token deleteConfirmation errorMsg, Effect.none )

        CapacityChange int ->
            ( Model ( roomID, { room | capacity = int } ) occupations backendUrl token deleteConfirmation errorMsg, Effect.none )

        NumberChange str ->
            ( Model ( roomID, { room | number = str } ) occupations backendUrl token deleteConfirmation errorMsg, Effect.none )

        UpdateRoomRequest ->
            ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg, Effect.sendCmd <| updateRoom ( roomID, room ) backendUrl token )

        UpdateRoomResult result ->
            case result of
                Ok ( id, updatedRoom ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model ( id, updatedRoom ) occupations backendUrl token deleteConfirmation errorMsg, Effect.updateRoom ( id, updatedRoom ) (Just route) )

                Err err ->
                    ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        DeleteRoomRequest ->
            if deleteConfirmation then
                ( Model ( roomID, room ) occupations backendUrl token False errorMsg, Effect.sendCmd <| deleteRoom roomID backendUrl token )

            else
                ( Model ( roomID, room ) occupations backendUrl token True errorMsg, Effect.none )

        DeleteRoomResult result ->
            case result of
                Ok _ ->
                    ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg, Effect.deleteRoom roomID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation "Erro no servidor. Verifique se ainda existem cadeiras (incluindo cadeiras escondidas) associadas a esta sala.", Effect.none )

                Err err ->
                    ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation (Decoders.errorToString err), Effect.none )

        GoToEditOccupationPage id ->
            ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.EditOccupation_Id_ { id = String.fromInt id }, query = Dict.empty, hash = Nothing } )

        GoToAddOccupationPage ->
            ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.AddOccupation, query = Dict.singleton "roomID" (String.fromInt roomID), hash = Nothing } )

        Return ->
            ( Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg) =
    let
        orderedOccupations =
            occupations
                |> Dict.toList
                |> List.sortWith (\( _, r1 ) ( _, r2 ) -> weekTimeComparator r1.start_time r2.start_time)
    in
    { title = "Editar Sala"
    , body =
        [ input [ class "input-box", style "width" "100%", value room.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value room.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value <| String.fromInt room.capacity, onInput (CapacityChange << Maybe.Extra.withDefaultLazy (\() -> room.capacity) << String.toInt), Html.Attributes.placeholder "Capacidade" ] []
        , input [ class "input-box", style "width" "100%", value room.number, onInput NumberChange, Html.Attributes.placeholder "Número" ] []
        , renderOccupations orderedOccupations
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


renderOccupations : List ( OccupationID, Occupation ) -> Html Msg
renderOccupations occupations =
    ul [ class "list2 custom-scrollbar" ] (ul [ ariaLabel "Ocupações", class "list-title" ] [ div [ class "gg-add", onClick GoToAddOccupationPage ] [] ] :: List.map renderOccupation occupations)


renderOccupation : ( OccupationID, Occupation ) -> Html Msg
renderOccupation ( id, occupation ) =
    li [ class "list-item", onClick (GoToEditOccupationPage id) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| convertWeekDay <| Just occupation.start_time ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| (++) "\t" <| (convertWeekTimeHourAndMinute <| Just occupation.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| (++) "\t" <| convertWeekTimeHourAndMinute <| Just occupation.end_time ]
        ]



------------------------ HTTP ------------------------


updateRoom : ( RoomID, Room ) -> String -> Token -> Cmd Msg
updateRoom ( id, room ) backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putRoom Nothing room)
        , expect = Http.expectJson UpdateRoomResult (Decoders.responseParser Decoders.getRoomAndID)
        , timeout = Nothing
        , tracker = Nothing
        }


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
