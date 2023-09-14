module RenderMain.View exposing (..)

import Dict
import DnD
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra
import RenderMain.List exposing (renderAvailableRooms, renderBlocks, renderEvents, renderLecturers, renderRooms)
import RenderMain.Model exposing (Model(..))
import RenderMain.Msg exposing (Msg(..))
import RenderMain.Schedule exposing (..)
import ScheduleObjects.Id exposing (ID)


view : Model -> Html Msg
view (Model data filters draggable selectedItems) =
    let
        tableWidth =
            90 / (3 |> toFloat) |> floor

        -- To shorten the function call;
        renderScheduleAbbr =
            renderSchedule tableWidth draggable

        roomList =
            Dict.filter filters.room data.events |> Dict.toList

        lectList =
            Dict.filter filters.lect data.events |> Dict.toList

        blockList =
            Dict.filter filters.block data.events |> Dict.toList

        -- Here we render the filters, turning them from (EventID -> Event -> Bool) into a List (EventID, Event)
        occupationsList =
            Dict.filter filters.occupations data.occupations |> Dict.toList

        restrictionList =
            Dict.filter filters.restrictions data.restrictions |> Dict.toList

        roomName =
            Maybe.map (\( _, r ) -> r.abbr) selectedItems.room |> Maybe.Extra.withDefaultLazy (\() -> "")

        lectName =
            Maybe.map (\( _, l ) -> l.abbr) selectedItems.lect |> Maybe.Extra.withDefaultLazy (\() -> "")

        blockName =
            Maybe.map (\( _, b ) -> b.nameAbbr) selectedItems.block |> Maybe.Extra.withDefaultLazy (\() -> "")

        displayOnDrag : ID -> Html Msg
        displayOnDrag id =
            div [] [ id |> String.fromInt |> text ]
    in
    div []
        [ div [ class "listbox-area" ]
            [ renderBlocks data.blocks data.hiddenBlocks selectedItems.block
            , renderLecturers data.lecturers data.hiddenLecturers selectedItems.lect
            , renderRooms data.rooms data.hiddenRooms selectedItems.room
            , renderEvents (Dict.toList data.events) (Dict.toList data.hiddenEvents) data.rooms data.lecturers selectedItems.event
            , renderAvailableRooms selectedItems.event data.rooms (Dict.values data.events) (Dict.values data.occupations)
            ]
        , div [ class "grids-container" ] [ renderScheduleAbbr blockList [] [] ("Bloco:" ++ roomName), renderScheduleAbbr roomList occupationsList [] ("Sala:" ++ roomName), renderScheduleAbbr lectList [] restrictionList ("Docente:" ++ lectName) ]
        , DnD.dragged
            draggable
            displayOnDrag
        ]
