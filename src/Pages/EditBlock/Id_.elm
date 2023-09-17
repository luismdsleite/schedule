module Pages.EditBlock.Id_ exposing (Model, Msg, page)

import Css
import Decoders
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
import ScheduleObjects.Block exposing (Block, BlockID, setBlockAbbr, setBlockCond, setBlockName)
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Hide exposing (IsHidden)
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


type alias Model =
    { data : Data
    , blockID : BlockID
    , block : Block
    , evList : EventList
    , deleteConfirmation : Bool
    , errorMsg : String
    , isHidden : Bool
    }



-- Model data ( blockID, block ) evList deleteConfirmation errorMsg isHidden


type alias EventList =
    { selectState : Select.State
    , items : List ( EventID, Event )
    , selectedEvents : List ( EventID, Event )
    }


asBlockIn : { a | block : b } -> b -> { a | block : b }
asBlockIn a block =
    { a | block = block }


setBlock block a =
    { a | block = block }


setEvList evList model =
    { model | evList = evList }


setEvListSelectState state evList =
    { evList | selectState = state }


setEvListSelectEv selectedEvents evList =
    { evList | selectedEvents = selectedEvents }


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
    ( Model data blockID block (initEventList ( blockID, block ) data) False "" isHidden
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
update msg model =
    case msg of
        SelectEvent selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.evList.selectState

                updatedSelectedEvents =
                    case maybeAction of
                        Just (Select.Select ( id, ev )) ->
                            ( id, ev ) :: model.evList.selectedEvents

                        Just (Select.Deselect deselectedItems) ->
                            List.filter (\( id, ev ) -> not (List.member ( id, ev ) deselectedItems)) model.evList.selectedEvents

                        Just Clear ->
                            []

                        _ ->
                            model.evList.selectedEvents

                newBlockCond =
                    \id _ -> List.member id (List.map Tuple.first updatedSelectedEvents)
            in
            ( model
                |> setBlock (model.block |> setBlockCond newBlockCond)
                |> setEvList (model.evList |> setEvListSelectState updatedSelectState |> setEvListSelectEv updatedSelectedEvents)
            , Effect.sendCmd (Cmd.map SelectEvent selectCmds)
            )

        -- ( Model data ( blockID, { block | cond = newBlockCond } ) { evList | selectState = updatedSelectState, selectedEvents = updatedSelectedEvents } deleteConfirmation errorMsg isHidden, Effect.sendCmd (Cmd.map SelectEvent selectCmds) )
        NameChange newName ->
            ( model |> setBlock (model.block |> setBlockName newName), Effect.none )

        AbbrChange newAbbr ->
            ( model |> setBlock (model.block |> setBlockAbbr newAbbr), Effect.none )

        UpdateBlockRequest ->
            ( model, Effect.sendCmd <| updateBlock ( model.blockID, model.block ) model.isHidden model.data.events model.data.backendUrl model.data.token )

        DeleteBlockRequest ->
            if model.deleteConfirmation then
                ( { model | deleteConfirmation = False }, Effect.sendCmd <| deleteBlock model.blockID model.data.backendUrl model.data.token )

            else
                ( { model | deleteConfirmation = True }, Effect.none )

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
                    ( model, Effect.updateBlock ( id, updatedBlock ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

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
                    ( model, Effect.deleteBlock model.blockID (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        VisibilityChange newVisibility ->
            ( {model | isHidden = newVisibility}, Effect.none )

        Return ->
            let
                route =
                    { path = Route.Path.Main
                    , query = Dict.empty
                    , hash = Nothing
                    }
            in
            ( model, Effect.pushRoute route )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Editar Blocos"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.block.nameAbbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.block.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Bloco" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateBlockRequest, style "margin-top" "1vh", style "margin-bottom" "1vh" ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteBlockRequest ]
            [ text
                (if model.deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
        , Html.map SelectEvent (HS.toUnstyled <| renderEventsList model.data model.evList)
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
