module RenderMain.Schedule exposing (renderSchedule)

{-| Renders a schedule 
-}

import RenderMain.DisplayEvents exposing (..)
import DnD
import Html exposing (..)
import Html.Attributes exposing (..)
import ScheduleObjects.Event exposing (Event)
import ScheduleObjects.WeekTime exposing (..)
import ScheduleObjects.WeekTimeConverters exposing (..)
import RenderMain.Msg exposing (..)
import ScheduleObjects.Event exposing (EventID)

renderSchedule : Int -> Draggable -> List ( EventID, Event ) -> String -> Html Msg
renderSchedule tableWidth draggable events title =
    let
        -- nothing22 =
        --     Debug.log "--------------" title
        -- nothing33 =
        --     Debug.log "List of (ID,Event)" events
        widthStr =
            String.append (tableWidth |> String.fromInt) "%"

        -- Function to create Columns headers li.
        weekdayToHtml weekDay =
            li [ class ("day " ++ (weekDay |> toCssClassWeekDay)) ] [ weekDay |> toPortugueseWeekday |> text ]

        -- Creating Row Header e.g: List ["08:00", "08:30", .. , "19:00", "19:30"].
        timeblocksText =
            let
                -- List [8,8,9,9,10,10,..,18,18,19,19]
                hours =
                    List.sort (List.range 8 19 ++ List.range 8 19)

                -- List [0,30,0,30,30, ...]
                minutes =
                    List.indexedMap
                        (\index _ ->
                            if modBy 2 index == 0 then
                                0

                            else
                                30
                        )
                        (List.repeat 24 0)
            in
            List.map2 convertHourAndMinute hours minutes

        timeblocks =
            List.map2 (\index str -> li [ class ("time t" ++ String.fromInt index) ] [ text str ]) (List.range 0 23) timeblocksText

        -- Empty slots (with no events). They possess a event listener that triggers on the case of an event that is dragged to it (Triggers a Msg onDrop).
        liWeekSlots =
            let
                -- List [8,8,9,9,10,10,..,18,18,19,19]
                hours =
                    List.sort (List.range startingHour endingHour ++ List.range startingHour endingHour)

                getMinute index =
                    if modBy 2 index == 0 then
                        0

                    else
                        30

                -- Create empty slots for a line of a schedule
                lineWeekLi =
                    \index hour -> List.map (\weekday -> li [] [ dnd.droppable ( RoomEvent 1, WeekTime weekday hour (getMinute index) ) [ style "height" "100%", style "width" "100%" ] [] ]) displayedWeekDays

                -- lineWeekLi = (\index hour -> List.map (\weekday -> li [] [ dnd.droppable ( RoomEvent 1, WeekTime weekday hour (getMinute index) ) [ style "height" "100%", style "width" "100%" ] [  Debug.toString weekday ++ " " ++ Debug.toString hour ++ ":" ++ Debug.toString (getMinute index) |> text ] ]) displayedWeekDays)
            in
            List.indexedMap lineWeekLi hours |> List.concat

        -- Display Events Cells to be renders
        liDisplayEvents =
            let
                -- Events separated based on their Time.Weekday.
                evSortedByDays =
                    List.map (sortByWeekday events) displayedWeekDays

                -- Display Events separated based on their Time.Weekday.
                dEvSortedByDays =
                    List.map createDisplayEvents evSortedByDays

                -- Function to render all display events of a certain day
                renderDayDisplayEvents : ( List DisplayEvent, Int ) -> List (Html Msg)
                renderDayDisplayEvents ( dEvents, colLength ) =
                    List.map (renderDisplayEvent colLength draggable) dEvents
            in
            List.foldl (++) [] (List.map renderDayDisplayEvents dEvSortedByDays)
    in
    div [ style "width" widthStr ]
        [ h3 [ style "margin" "unset" ] [ text title ]
        , ul [ class "calendar weekly-byhour", style "width" widthStr ]
            -- (List.map weekdayToHtml displayedWeekDays ++ timeblocks ++ List.repeat (24 * 5) (li [] []) ++ liDisplayEvents)
            (List.map weekdayToHtml displayedWeekDays ++ timeblocks ++ liWeekSlots ++ liDisplayEvents)
        ]

{-| Turns a Display Event into a HTML <li> tag.
ColLength corresponds to the maximum number of colision between events in a day.
-}
renderDisplayEvent : Int -> Draggable -> DisplayEvent -> Html Msg
renderDisplayEvent colLength draggable (DisplayEvent id ev dInfo) =
    let
        width =
            ((dInfo.colEnd + 1) - dInfo.colStart) * 100 // colLength

        leftMargin =
            (dInfo.colStart * 100) // colLength

        weekday =
            toCssClassWeekDay dInfo.day

        {- Feature to Improve Visibility. Makes event visibility priority go from left to right -}
        zIndex =
            String.fromInt (999 - dInfo.colStart)

        {- Hide DisplayEvents when we want to drop a displayEvent -}
        hideAtt =
            case DnD.getDragMeta draggable of
                Just _ ->
                    [ style "pointer-events" "none" ]

                _ ->
                    []
    in
    if List.member dInfo.day displayedWeekDays then
        li ([ class "event work", style "style" ("grid-column: " ++ weekday), style "margin-left" (String.fromInt leftMargin ++ "%"), style "grid-row" ("t" ++ String.fromInt dInfo.lineStart ++ "   /  t" ++ String.fromInt dInfo.lineEnd), style "width" (String.fromInt width ++ "%"), style "grid-column" weekday, style "z-index" zIndex, attribute "title" ev.subjectAbbr ] ++ hideAtt) [ dnd.draggable id [ style "height" "-webkit-fill-available", style "width" "-webkit-fill-available" ] [ text ev.subjectAbbr ] ]

    else
        li [ style "display" "none" ] [ text ev.subjectAbbr ]
