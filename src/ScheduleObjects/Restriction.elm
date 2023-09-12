module ScheduleObjects.Restriction exposing (Category(..), Restriction, RestrictionID, categoryToColor, restrictionToPortugueseString)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Restriction =
    { lect : LecturerID
    , start_time : WeekTime
    , end_time : WeekTime
    , category : Category
    }


type alias RestrictionID =
    ID


type Category
    = Priority
    | Service
    | Preference
    | Other


categoryToColor : Category -> String
categoryToColor category =
    case category of
        Priority ->
            "orange"

        Service ->
            "red"

        Preference ->
            "green"

        Other ->
            "yellow"


restrictionToPortugueseString : Restriction -> String
restrictionToPortugueseString restriction =
    case restriction.category of
        Priority ->
            "Prioridade"

        Service ->
            "Serviço"

        Preference ->
            "Preferência"

        Other ->
            "Outro"
