-- TODO: Expose only calculateDisplayEvents.


module DisplayEvents exposing (..)

import Array
import Matrix exposing (..)
import Maybe exposing (andThen)
import ScheduleObjects exposing (Event)


{-| Wrapper around the Event variable adding only the necessary info to display it
-}
type DisplayEvent
    = DisplayEvent Event { lineStart : Int, lineEnd : Int, colStart : Int, colEnd : Int }


{-| Checks if an event can fit in a certain column without overlapping with other events.
-}
doesEvFitInCol : DisplayEvent -> Int -> Matrix Int -> Bool
doesEvFitInCol (DisplayEvent _ evInfo) colIndex colisionGrid =
    -- If out of bounds, return False
    if colIndex == -1 || colIndex >= (Tuple.first <| size <| colisionGrid) then
        False
        -- Else check if there exists any event already in the same range

    else
        Array.foldr (+) 0 (Matrix.getYs colisionGrid colIndex |> Array.slice evInfo.lineStart evInfo.lineEnd) == 0


{-| After knowing in what columns each event will be (without overlap), we figure out how much empty space exists to the left of the event (without overlapping with other events) and expand it.
-}
evLeftExp : DisplayEvent -> Int -> Matrix Int -> Int
evLeftExp displayEv colIndex colisionGrid =
    if doesEvFitInCol displayEv (colIndex - 1) colisionGrid then
        evLeftExp displayEv (colIndex - 1) colisionGrid

    else
        colIndex


{-| After knowing in what columns each event will be (without overlap), we figure out how much empty space exists to the right of the event (without overlapping with other events) and expand it.
-}
evRightExp : DisplayEvent -> Int -> Matrix Int -> Int
evRightExp displayEv colIndex colisionGrid =
    if doesEvFitInCol displayEv (colIndex + 1) colisionGrid then
        evRightExp displayEv (colIndex + 1) colisionGrid

    else
        colIndex


{-| TODO: finish this function and expose it.
INFO: If an event has a start\_time and a end\_time else dont display it!
-}
calculateDisplayEvents : List ( Int, Event ) -> List ( Int, DisplayEvent )
calculateDisplayEvents evList =
    let
        -- Events need to possess both a start date and an end date, otherwise they cannot be displayed!
        validEvents =
            -- This can be reduced! I simply do not know how, I suspect it's with the use of Maybe.AndThen().
            List.filter
                (\( _, ev ) ->
                    case ev.start_time of
                        Just _ ->
                            case ev.end_time of
                                Just _ ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
                )
                evList
    in
    []
