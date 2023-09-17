module Pages.AddBlock exposing (Model, Msg, page)

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


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { data : Data, block : Block, evList : EventList, deleteConfirmation : Bool, errorMsg : String, isHidden : Bool }


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


type alias EventList =
    { selectState : Select.State
    , items : List ( EventID, Event )
    , selectedEvents : List ( EventID, Event )
    }


init : Data -> () -> ( Model, Effect Msg )
init data () =
    ( Model data (Block "" "" (\_ _ -> False)) (initEventList data) False "" False
    , Effect.none
    )


initEventList : Data -> EventList
initEventList data =
    let
        items =
            Dict.toList data.events
    in
    { selectState =
        Select.initState (Select.selectIdentifier "Event")
            |> Select.keepMenuOpen True
    , items = items
    , selectedEvents = []
    }



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | SelectEvent (Select.Msg ( EventID, Event ))
    | UpdateBlockRequest
    | UpdateBlockResult (Result Http.Error ( BlockID, ( Block, IsHidden ) ))
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

        NameChange newName ->
            ( setBlockName newName model.block |> asBlockIn model, Effect.none )

        AbbrChange newAbbr ->
            ( setBlockAbbr newAbbr model.block |> asBlockIn model, Effect.none )

        UpdateBlockRequest ->
            ( model, Effect.sendCmd <| updateBlock model.block model.isHidden model.data.events model.data.backendUrl model.data.token )

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

        VisibilityChange newVisibility ->
            ( { model | isHidden = newVisibility }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Criar Bloco"
    , body =
        [ input [ class "input-box", style "width" "100%", value model.block.nameAbbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.block.name, onInput NameChange, Html.Attributes.placeholder "Nome Da Sala" ] []
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Bloco" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateBlockRequest, style "margin-top" "1vh", style "margin-bottom" "1vh" ] [ text "Submeter" ]
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


updateBlock : Block -> IsHidden -> Dict EventID Event -> String -> Token -> Cmd Msg
updateBlock block isHidden events backendUrl token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "blocks"
        , body = Http.jsonBody (Encoders.putBlock Nothing events block isHidden)
        , expect = Http.expectJson UpdateBlockResult (Decoders.responseParser Decoders.getBlockAndID)
        , timeout = Nothing
        , tracker = Nothing
        }
