module ScheduleObjects.Restriction exposing (Category(..), Restriction, RestrictionID, allCategories, asRestrictionIn, categoryComparator, categoryToColor, categoryToPortugueseString, setRestCategory, setRestEndTime, setRestLect, setRestStartTime, setRestriction)

import ScheduleObjects.Id exposing (ID)
import ScheduleObjects.Lecturer exposing (LecturerID)
import ScheduleObjects.WeekTime exposing (WeekTime)


type alias Restriction =
    { lect : LecturerID
    , start_time : WeekTime
    , end_time : WeekTime
    , category : Category
    }


asRestrictionIn : { a | restriction : Restriction } -> Restriction -> { a | restriction : Restriction }
asRestrictionIn a restriction =
    { a | restriction = restriction }


setRestriction : Restriction -> { b | restriction : Restriction } -> { b | restriction : Restriction }
setRestriction restriction a =
    { a | restriction = restriction }


setRestCategory : Category -> Restriction -> Restriction
setRestCategory category restriction =
    { restriction | category = category }


setRestStartTime : WeekTime -> Restriction -> Restriction
setRestStartTime start_time restriction =
    { restriction | start_time = start_time }


setRestEndTime : WeekTime -> Restriction -> Restriction
setRestEndTime end_time restriction =
    { restriction | end_time = end_time }


setRestLect : LecturerID -> Restriction -> Restriction
setRestLect lect restriction =
    { restriction | lect = lect }


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
