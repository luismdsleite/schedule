module SelectLists.Hour exposing (HourList, initHourList, renderHourSelect)

{-| This module contains the functions to create and render a select list for the hours of the day.
-}

import Html.Styled
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.WeekTimeConverters exposing (computeTimeSlots, convertHourAndMinute)
import Select exposing (basicMenuItem)
import Time


type alias MaybeHourList =
    { selectState : Select.State
    , items : List (Select.MenuItem ( Int, Int ))
    , selectedHour : Maybe ( Int, Int )
    }


initHourList eventHour selectIdentifier =
    let
        hours =
            computeTimeSlots (WeekTime Time.Mon 8 0) (WeekTime Time.Mon 20 0) []
                |> List.map (\a -> ( a.hour, a.minute ))
                |> List.reverse
    in
    { selectState =
        Select.initState (Select.selectIdentifier selectIdentifier)
    , items =
        List.map (\( hour, minute ) -> basicMenuItem { item = ( hour, minute ), label = convertHourAndMinute hour minute }) hours
    , selectedHour = eventHour
    }


renderMaybeHourSelect : MaybeHourList -> String -> Html.Styled.Html (Select.Msg ( Int, Int ))
renderMaybeHourSelect hourList placeholder =
    Select.view
        ((Select.single <| Maybe.map (\( hour, minute ) -> basicMenuItem { item = ( hour, minute ), label = convertHourAndMinute hour minute }) hourList.selectedHour)
            |> Select.state hourList.selectState
            |> Select.menuItems hourList.items
            |> Select.placeholder placeholder
        )


type alias HourList =
    { selectState : Select.State
    , items : List (Select.MenuItem ( Int, Int ))
    , selectedHour : ( Int, Int )
    }


renderHourSelect : HourList -> String -> Html.Styled.Html (Select.Msg ( Int, Int ))
renderHourSelect hourList placeholder =
    Select.view
        ((Select.single <| (\( hour, minute ) -> Just (basicMenuItem { item = ( hour, minute ), label = convertHourAndMinute hour minute })) hourList.selectedHour)
            |> Select.state hourList.selectState
            |> Select.menuItems hourList.items
            |> Select.placeholder placeholder
        )
