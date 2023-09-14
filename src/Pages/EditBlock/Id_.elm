module Pages.EditBlock.Id_ exposing (Model, Msg, page)

import Css
import Decoders exposing (IsHidden)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Block exposing (Block, BlockID)
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Room exposing (Room, RoomID)
import ScheduleObjects.WeekTimeConverters exposing (..)
import Select exposing (..)
import Select.Styles as Styles
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
    = Model Data ( BlockID, Block ) EventList Bool String Bool


type alias EventList =
    { selectState : Select.State
    , items : List ( EventID, Event )
    , selectedEvents : List ( EventID, Event )
    }


init : Data -> String -> () -> ( Model, Effect Msg )
init data eventIDParam () =
    let
        blockID =
            case String.toInt eventIDParam of
                Just number ->
                    number

                Nothing ->
                    -1

        ( block, isHidden ) =
            case Dict.get blockID data.blocks of
                Just b ->
                    ( b, False )

                Nothing ->
                    ( Dict.get blockID data.hiddenBlocks |> Maybe.Extra.withDefaultLazy (\() -> Block "" "" (\_ _ -> False)), True )
    in
    ( Model data ( blockID, block ) (initEventList ( blockID, block ) data) False "" isHidden
    , Effect.none
    )


initEventList : ( BlockID, Block ) -> Data -> EventList
initEventList ( blockID, block ) data =
    let
        eventIDsOfBlock =
            Dict.filter block.cond data.events
                |> Dict.toList

        items =
            Dict.toList data.events
    in
    { selectState =
        Select.initState (Select.selectIdentifier "Event")
            |> Select.keepMenuOpen True
    , items = items
    , selectedEvents = eventIDsOfBlock
    }



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | SelectEvent (Select.Msg ( EventID, Event ))
    | UpdateBlockRequest
    | UpdateBlockResult (Result Http.Error ( BlockID, ( Block, IsHidden ) ))
    | DeleteBlockRequest
    | DeleteBlockResult (Result Http.Error ())
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden) =
    case msg of
        SelectEvent selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg evList.selectState

                updatedSelectedEvents =
                    case maybeAction of
                        Just (Select.Select ( id, ev )) ->
                            ( id, ev ) :: evList.selectedEvents

                        Just (Select.Deselect deselectedItems) ->
                            List.filter (\( id, ev ) -> not (List.member ( id, ev ) deselectedItems)) evList.selectedEvents

                        Just Clear ->
                            []

                        _ ->
                            evList.selectedEvents

                newBlockCond =
                    \id _ -> List.member id (List.map Tuple.first updatedSelectedEvents)
            in
            ( Model data ( blockID, { block | cond = newBlockCond } ) { evList | selectState = updatedSelectState, selectedEvents = updatedSelectedEvents } deleteConfirmation errorMsg isHidden, Effect.sendCmd (Cmd.map SelectEvent selectCmds) )

        NameChange newName ->
            ( Model data ( blockID, { block | name = newName } ) evList deleteConfirmation errorMsg isHidden, Effect.none )

        AbbrChange newAbbr ->
            ( Model data ( blockID, { block | nameAbbr = newAbbr } ) evList deleteConfirmation errorMsg isHidden, Effect.none )

        UpdateBlockRequest ->
            ( Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden, Effect.sendCmd <| updateBlock ( blockID, block ) isHidden data.events data.backendUrl data.token )

        DeleteBlockRequest ->
            if deleteConfirmation then
                ( Model data ( blockID, block ) evList False errorMsg isHidden, Effect.sendCmd <| deleteBlock blockID data.backendUrl data.token )

            else
                ( Model data ( blockID, block ) evList True errorMsg isHidden, Effect.none )

        UpdateBlockResult result ->
            case result of
                Ok ( id, updatedBlock ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden, Effect.updateBlock ( id, updatedBlock ) (Just route) )

                Err err ->
                    ( Model data ( blockID, block ) evList deleteConfirmation (Decoders.errorToString err) isHidden, Effect.none )

        DeleteBlockResult result ->
            case result of
                Ok () ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden, Effect.deleteBlock blockID (Just route) )

                Err err ->
                    ( Model data ( blockID, block ) evList deleteConfirmation (Decoders.errorToString err) isHidden, Effect.none )

        VisibilityChange newVisibility ->
            ( Model data ( blockID, block ) evList deleteConfirmation errorMsg newVisibility, Effect.none )

        Return ->
            let
                route =
                    { path = Route.Path.Main
                    , query = Dict.empty
                    , hash = Nothing
                    }
            in
            ( Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden, Effect.pushRoute route )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden) =
    { title = "Editar Blocos"
    , body =
        [ input [ class "input-box", style "width" "100%", value block.nameAbbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value block.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , div [] [ input [ type_ "checkbox", checked isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Bloco" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateBlockRequest, style "margin-top" "1vh", style "margin-bottom" "1vh" ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteBlockRequest ]
            [ text
                (if deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        , Html.map SelectEvent (HS.toUnstyled <| renderEventsList data evList)
        ]
    }


renderEventsList : Data -> EventList -> HS.Html (Select.Msg ( EventID, Event ))
renderEventsList data eventList =
    let
        selectedItems =
            List.map (\( id, ev ) -> Select.basicMenuItem { item = ( id, ev ), label = ev.subjectAbbr }) eventList.selectedEvents

        menuItems =
            List.map (\( id, ev ) -> Select.customMenuItem { item = ( id, ev ), label = ev.subjectAbbr, view = renderEvent data.rooms data.lecturers ( id, ev ) }) eventList.items
    in
    Select.view
        (Select.multi selectedItems
            |> Select.state eventList.selectState
            |> Select.menuItems menuItems
            |> Select.placeholder "Selecione os eventos"
            |> Select.searchable True
            |> Select.clearable True
            |> Select.setStyles (Styles.default |> Styles.setMenuStyles menuBranding)
        )


menuBranding : Styles.MenuConfig
menuBranding =
    Styles.getMenuConfig Styles.default
        |> Styles.setMenuMaxHeightVh (Css.vh 50)


{-| Transforms an event into a list item
-}
renderEvent : Dict RoomID Room -> Dict LecturerID Lecturer -> ( EventID, Event ) -> HS.Html Never
renderEvent rooms lecturers ( eventID, event ) =
    let
        room =
            case event.room of
                Just roomID ->
                    case Dict.get roomID rooms of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Room "----" "----" -1 "----"

                -- Event still has no room assigned
                Nothing ->
                    Room "----" "----" -1 "----"

        lecturer =
            case event.lecturer of
                Just lecturerID ->
                    case Dict.get lecturerID lecturers of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Lecturer "----" "----" ""

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" ""
    in
    HS.li [ HSA.class "list-item" ]
        [ HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "10%", HSA.attribute "title" event.subjectAbbr ] [ HS.text event.subjectAbbr ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "35%", HSA.attribute "title" event.subject, HSA.style "margin-left" "1%" ] [ HS.text event.subject ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "5%", HSA.style "margin-left" "1%" ] [ HS.text (convertWeekDay event.start_time) ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "10%", HSA.style "margin-left" "1%" ] [ HS.text (convertWeekTimeHourAndMinute event.start_time) ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "10%", HSA.style "margin-left" "1%" ] [ HS.text (convertWeekTimeHourAndMinute event.end_time) ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "15%", HSA.attribute "title" room.abbr, HSA.style "margin-left" "1%" ] [ HS.text room.abbr ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "5%", HSA.style "margin-left" "1%" ] [ HS.text (String.fromInt room.capacity) ]
        , HS.div [ HSA.class "custom-scrollbar", HSA.class "list-text", HSA.style "width" "10%", HSA.style "margin-left" "1%" ] [ HS.text lecturer.abbr ]
        ]



------------------------ HTTP ------------------------


updateBlock : ( BlockID, Block ) -> IsHidden -> Dict EventID Event -> String -> Token -> Cmd Msg
updateBlock ( id, block ) isHidden events backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "blocks\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putBlock Nothing events block isHidden)
        , expect = Http.expectJson UpdateBlockResult (Decoders.responseParser Decoders.getBlockAndID)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteBlock : BlockID -> String -> Token -> Cmd Msg
deleteBlock id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "blocks\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteBlockResult (Ok ())

        Err err ->
            DeleteBlockResult (Err err)
