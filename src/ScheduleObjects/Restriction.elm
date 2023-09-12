module ScheduleObjects.Restriction exposing (Category(..), Restriction, RestrictionID, allCategories, categoryComparator, categoryToColor, categoryToPortugueseString)

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


categoryToPortugueseString : Category -> String
categoryToPortugueseString category =
    case category of
        Priority ->
            "Prioridade 🟧"

        Service ->
            "Serviço 🟥"

        Preference ->
            "Preferência 🟩"

        Other ->
            "Outro 🟨"


allCategories : List Category
allCategories =
    [ Priority, Service, Preference, Other ]


categoryComparator : Category -> Category -> Order
categoryComparator category1 category2 =
    let
        categoryToNumber category =
            case category of
                Priority ->
                    1

                Service ->
                    2

                Preference ->
                    3

                Other ->
                    4
    in
    compare (categoryToNumber category1) (categoryToNumber category2)
