module ScheduleObjects.Lecturer exposing (Lecturer, LecturerID, asLectIn, setLect, setLectAbbr, setLectName, setLectOffice)

{-| A lecturer/teacher has an ID, a name, a abbreviation, times of availibility and a office.
-}

import ScheduleObjects.Id exposing (ID)


type alias Lecturer =
    { name : String, abbr : String, office : String }


setLect : a -> { b | lect : a } -> { b | lect : a }
setLect lect a =
    { a | lect = lect }


asLectIn : { a | lect : b } -> b -> { a | lect : b }
asLectIn a lect =
    { a | lect = lect }


setLectName : String -> Lecturer -> Lecturer
setLectName name lecturer =
    { lecturer | name = name }


setLectAbbr : String -> Lecturer -> Lecturer
setLectAbbr abbr lecturer =
    { lecturer | abbr = abbr }


setLectOffice : String -> Lecturer -> Lecturer
setLectOffice office lecturer =
    { lecturer | office = office }


type alias LecturerID =
    ID
