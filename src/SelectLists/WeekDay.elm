module SelectLists.WeekDay exposing (MaybeWeekDayList, WeekDayList, initWeekDayList, renderMaybeWeekdaySelect, renderWeekdaySelect, setWeekdayList, setWeekdayListSelect, setWeekdaySelectState)

import Html.Styled
import ScheduleObjects.WeekTime exposing (WeekTime)
import ScheduleObjects.WeekTimeConverters exposing (..)
import Select exposing (basicMenuItem)
import Time


type alias WeekDayList =
    { selectState : Select.State
    , items : List (Select.MenuItem Time.Weekday)
    , selectedWeekday : Time.Weekday
    }


type alias MaybeWeekDayList =
    { selectState : Select.State
    , items : List (Select.MenuItem Time.Weekday)
    , selectedWeekday : Maybe Time.Weekday
    }


setWeekdayList weekdayList a =
    { a | weekdayList = weekdayList }


setWeekdaySelectState state weekdayList =
    { weekdayList | selectState = state }


setWeekdayListSelect selectedEvents weekdayList =
    { weekdayList | selectedWeekday = selectedEvents }


initWeekDayList : a -> { selectState : Select.State, items : List (Select.MenuItem Time.Weekday), selectedWeekday : a }
initWeekDayList selectedWeekday =
    { selectState =
        Select.initState (Select.selectIdentifier "Weekday")
    , items =
        List.map (\weekday -> basicMenuItem { item = weekday, label = toPortugueseWeekday weekday }) displayedWeekDays
    , selectedWeekday = selectedWeekday
    }


renderMaybeWeekdaySelect : MaybeWeekDayList -> Html.Styled.Html (Select.Msg Time.Weekday)
renderMaybeWeekdaySelect weekdayList =
    Select.view
        ((Select.single <| Maybe.map (\weekday -> basicMenuItem { item = weekday, label = toPortugueseWeekday weekday }) weekdayList.selectedWeekday)
            |> Select.clearable True
            |> Select.state weekdayList.selectState
            |> Select.menuItems weekdayList.items
            |> Select.placeholder "Dia da Semana"
        )


renderWeekdaySelect : WeekDayList -> Html.Styled.Html (Select.Msg Time.Weekday)
renderWeekdaySelect weekdayList =
    Select.view
        ((Select.single <| Just (basicMenuItem { item = weekdayList.selectedWeekday, label = toPortugueseWeekday weekdayList.selectedWeekday }))
            |> Select.state weekdayList.selectState
            |> Select.menuItems weekdayList.items
            |> Select.placeholder "Dia da Semana"
        )
