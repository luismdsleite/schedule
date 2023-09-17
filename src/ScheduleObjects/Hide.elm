module ScheduleObjects.Hide exposing (..)


type alias IsHidden =
    Bool


setHidden : a -> { b | isHidden : a } -> { b | isHidden : a }
setHidden isHidden model =
    { model | isHidden = isHidden }
