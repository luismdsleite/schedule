module RenderMain.View exposing (..)

import Dict
import DnD
import Html exposing (..)
import Html.Attributes exposing (..)
import RenderMain.List exposing (renderBlocks, renderEvents, renderLecturers, renderRooms)
import RenderMain.Model exposing (Model(..))
import RenderMain.Msg exposing (Msg(..))
import RenderMain.Schedule exposing (..)
import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Occupation exposing (Occupation)


view : Model -> Html Msg
view (Model data filters draggable) =
    let
        tableWidth =
            90 / (3 |> toFloat) |> floor

        -- To shorten the function call
        renderScheduleAbbr =
            renderSchedule tableWidth draggable

        -- Here we render the filters, turning them from (EventID -> Event -> Bool) into a List (EventID, Event)
        roomList =
            Dict.filter filters.room data.events |> Dict.toList

        lectList =
            Dict.filter filters.lect data.events |> Dict.toList

        blockList =
            Dict.filter filters.block data.events |> Dict.toList

        occupationsList =
            Dict.filter filters.occupations data.occupations |> Dict.toList

        restrictionList =
            Dict.filter filters.restrictions data.restrictions |> Dict.toList

        displayOnDrag : ID -> Html Msg
        displayOnDrag id =
            div [] [ id |> Debug.toString |> text ]
    in
    div []
        [ div [ class "listbox-area" ]
            [ renderBlocks data.blocks
            , renderLecturers data.lecturers
            , renderRooms data.rooms
            , renderEvents (Dict.toList data.events) data.rooms data.lecturers
            ]
        , div [ class "grids-container" ] [ renderScheduleAbbr blockList [] [] ("Bloco:" ++ filters.blockName), renderScheduleAbbr roomList occupationsList [] ("Sala:" ++ filters.roomName), renderScheduleAbbr lectList [] restrictionList ("Docente:" ++ filters.lectName) ]
        , DnD.dragged
            draggable
            displayOnDrag
        ]
