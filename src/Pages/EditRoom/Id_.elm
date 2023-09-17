module Pages.EditRoom.Id_ exposing (Model, Msg, page)

import Decoders
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Occupation exposing (Occupation, OccupationID)
import ScheduleObjects.Room exposing (Room, RoomID, asRoomIn, setRoomAbbr, setRoomCapacity, setRoomName, setRoomNumber)
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


type alias Model =
    { roomID : RoomID
    , room : Room
    , occupations : Dict OccupationID Occupation
    , backendUrl : String
    , token : Token
    , deleteConfirmation : Bool
    , errorMsg : String
    , isHidden : IsHidden
    }


init : Data -> String -> () -> ( Model, Effect Msg )
init data roomIDParam () =
    let
        roomID =
            String.toInt roomIDParam
                |> Maybe.Extra.withDefaultLazy (\_ -> -1)

        ( room, isHidden ) =
            case Dict.get roomID data.rooms of
                Just r ->
                    ( r, False )

                _ ->
                    ( Dict.get roomID data.hiddenRooms |> Maybe.Extra.withDefaultLazy (\() -> Room "" "" 0 ""), True )
    in
    ( Model roomID room (Dict.filter (\_ occupations -> occupations.room == roomID) data.occupations) data.backendUrl data.token False "" isHidden, Effect.none )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | CapacityChange Int
    | NumberChange String
    | UpdateRoomRequest
    | UpdateRoomResult (Result Http.Error ( RoomID, ( Room, IsHidden ) ))
    | DeleteRoomRequest
    | DeleteRoomResult (Result Http.Error ())
    | GoToEditOccupationPage OccupationID
    | GoToAddOccupationPage
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    -- Model ( roomID, room ) occupations backendUrl token deleteConfirmation errorMsg isHidden =
    case msg of
        AbbrChange str ->
            ( setRoomAbbr str model.room |> asRoomIn model, Effect.none )

        NameChange str ->
            ( setRoomName str model.room |> asRoomIn model, Effect.none )

        CapacityChange int ->
            ( setRoomCapacity int model.room |> asRoomIn model, Effect.none )

        NumberChange str ->
            ( setRoomNumber str model.room |> asRoomIn model, Effect.none )

        UpdateRoomRequest ->
            ( model, Effect.sendCmd <| updateRoom ( model.roomID, model.room ) model.isHidden model.backendUrl model.token )

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
                    ( model, Effect.updateRoom ( id, updatedRoom ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        DeleteRoomRequest ->
            if model.deleteConfirmation then
                ( { model | deleteConfirmation = False }, Effect.sendCmd <| deleteRoom model.roomID model.backendUrl model.token )

            else
                ( { model | deleteConfirmation = True }, Effect.none )

        DeleteRoomResult result ->
            case result of
                Ok _ ->
                    ( model, Effect.deleteRoom model.roomID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( { model | errorMsg = "Erro no servidor. Verifique se ainda existem cadeiras (incluindo cadeiras escondidas) ou ocupações associadas a esta sala." }, Effect.none )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        GoToEditOccupationPage id ->
            ( model, Effect.pushRoute { path = Route.Path.EditOccupation_Id_ { id = String.fromInt id }, query = Dict.empty, hash = Nothing } )

        GoToAddOccupationPage ->
            ( model, Effect.pushRoute { path = Route.Path.AddOccupation, query = Dict.singleton "roomID" (String.fromInt model.roomID), hash = Nothing } )

        VisibilityChange bool ->
            ( { model | isHidden = bool }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        orderedOccupations =
            model.occupations
                |> Dict.toList
                |> List.sortWith (\( _, r1 ) ( _, r2 ) -> weekTimeComparator r1.start_time r2.start_time)
    in
    { title = "Editar Sala"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.room.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.room.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , input [ class "input-box", style "width" "100%", value <| String.fromInt model.room.capacity, onInput (CapacityChange << Maybe.Extra.withDefaultLazy (\() -> model.room.capacity) << String.toInt), Html.Attributes.placeholder "Capacidade" ] []
        , input [ class "input-box", style "width" "100%", value model.room.number, onInput NumberChange, Html.Attributes.placeholder "Número" ] []
        , renderOccupations orderedOccupations
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Docente" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateRoomRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteRoomRequest ]
            [ text
                (if model.deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
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


updateRoom : ( RoomID, Room ) -> IsHidden -> String -> Token -> Cmd Msg
updateRoom ( id, room ) isHidden backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "rooms\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putRoom Nothing room isHidden)
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
