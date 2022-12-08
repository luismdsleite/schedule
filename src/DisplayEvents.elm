-- TODO: Expose only createDisplayEvents.


module DisplayEvents exposing (..)

import Array exposing (Array, length)
import Matrix exposing (..)
import Maybe exposing (andThen)
import ScheduleObjects exposing (Event, WeekTime)
import Time exposing (..)


{-| Important Constants
-}
startingHour : Int
startingHour =
    8


endingHour : Int
endingHour =
    19


startingMinute : Int
startingMinute =
    0


endingMinute : Int
endingMinute =
    30


{-| Wrapper around the Event variable adding only the necessary info to display it
-}
type DisplayEvent
    = DisplayEvent Int Event DisplayInfo


{-| Aditional parameters needed to display an event
-}
type alias DisplayInfo =
    { day : Time.Weekday, lineStart : Int, lineEnd : Int, colStart : Int, colEnd : Int }


hash : WeekTime -> Int
hash weekTime =
    (weekTime.hour - startingHour)
        * 2
        + (if weekTime.minute >= 30 then
            1

           else
            0
          )



-- {-| Checks if an event can fit in a certain column without overlapping with other events.
-- -}
-- doesEvFitInCol : DisplayEvent -> Int -> Matrix Int -> Bool
-- doesEvFitInCol (DisplayEvent _ _ evInfo) colIndex colisionGrid =
--     -- If out of bounds, return False
--     if colIndex == -1 || colIndex >= (Tuple.first <| size <| colisionGrid) then
--         False
--         -- Else check if there exists any event already in the same range
--     else
--         Array.foldr (+) 0 (Matrix.getYs colisionGrid colIndex |> Array.slice evInfo.lineStart evInfo.lineEnd) == 0


{-| Checks if an event can fit in a certain column without overlapping with other events.
-}



-- TODO: Use List.any


doesEvFitInCol : Event -> Int -> Matrix Int -> Bool
doesEvFitInCol ev colIndex colisionGrid =
    -- If out of bounds, return False
    if colIndex == -1 || colIndex >= (Tuple.first <| size <| colisionGrid) then
        False
        -- Else check if there exists any event already in the same range

    else
        Maybe.withDefault False
            (ev.start_time
                |> Maybe.andThen
                    (\start_time ->
                        ev.end_time
                            |> Maybe.andThen (\end_time -> Just (Array.foldr (+) 0 (Matrix.getYs colisionGrid colIndex |> Array.slice (start_time |> hash) ((end_time |> hash) + 1)) == 0))
                    )
            )


{-| After knowing in what columns each event will be (without overlap), we figure out how much empty space exists to the left of the event (without overlapping with other events) and expand it.
-}
evLeftExp : DisplayEvent -> Int -> Matrix Int -> Int
evLeftExp (DisplayEvent id ev evInfo) colIndex colisionGrid =
    if doesEvFitInCol ev (colIndex - 1) colisionGrid then
        evLeftExp (DisplayEvent id ev evInfo) (colIndex - 1) colisionGrid

    else
        colIndex


{-| After knowing in what columns each event will be (without overlap), we figure out how much empty space exists to the right of the event (without overlapping with other events) and expand it.
-}
evRightExp : DisplayEvent -> Int -> Matrix Int -> Int
evRightExp (DisplayEvent id ev evInfo) colIndex colisionGrid =
    if doesEvFitInCol ev (colIndex + 1) colisionGrid then
        evRightExp (DisplayEvent id ev evInfo) (colIndex + 1) colisionGrid

    else
        colIndex


{-| TODO: finish this function and expose it.
INFO: If an event doesn't has a start\_time and a end\_time else dont display it!
-}
createDisplayEvents : List ( Int, Event ) -> ( List DisplayEvent, Int )
createDisplayEvents evList =
    let
        -- Outputs the lineEnd of the last event on the grid. This variable is used to define the grid size.
        gridLinesSize =
            List.foldr (\( _, ev1 ) prevMax -> max prevMax (hash (Maybe.withDefault (WeekTime Time.Mon startingHour startingMinute) ev1.end_time))) 0 evList

        gridColsSize =
            List.length evList

        ( dEvents, grid ) =
            getDisplayEvents evList [] (Matrix.initialize gridLinesSize gridColsSize (\_ _ -> 0)) 0

        -- Aux function to determine how many of the grids columns are actually being used.
        getLength : Int -> Array Int -> Int
        getLength index arr =
            if index <= -1 then
                0

            else
                case Array.get index arr of
                    Just id ->
                        if id == 0 then
                            getLength (index - 1) arr

                        else
                            index

                    Nothing ->
                        index

        realCols =
            Array.foldl max 0 (Array.map (getLength (gridColsSize - 1)) grid) + 1

        nothing3 =
            Debug.log "gridcolsSize" gridColsSize

        nothing4 =
            Debug.log "realCols" realCols
    in
    ( dEvents, realCols )



-- fillDisplayEventsCols : List (Int, DisplayEvent) ->


{-| Converts a list of Events to Display Events.
We treat evList and dEvList as stacks and will gradually in each iteration empty evList and fill dEvList.
-}
getDisplayEvents : List ( Int, Event ) -> List DisplayEvent -> Matrix Int -> Int -> ( List DisplayEvent, Matrix Int )
getDisplayEvents evList dEvList grid colIndex =
    case evList of
        -- If evList is not empty then take the head, transform it into a displayEvent, add it to dEvList and check again.
        ( id, ev ) :: tail ->
            -- If we're out of bounds ignore this event and proceed to the next event.
            if colIndex > (grid |> size |> Tuple.second) then
                getDisplayEvents tail dEvList grid 0

            else if doesEvFitInCol ev colIndex grid then
                -- Checking if the head fits the grid in the column colIndex. If it does were done, otherwise we check the next column.
                let
                    ( weekDay, lineStart ) =
                        case ev.start_time of
                            Just start_time ->
                                ( start_time.weekday, hash start_time )

                            Nothing ->
                                ( Time.Mon, -1 )

                    lineEnd =
                        case ev.end_time of
                            Just end_time ->
                                end_time |> hash

                            Nothing ->
                                -1

                    dEvent =
                        DisplayEvent id ev (DisplayInfo weekDay lineStart lineEnd colIndex colIndex)
                in
                -- Adding new Display Event and filling out the grid.
                getDisplayEvents tail (dEvent :: dEvList) (gridSetEvent dEvent grid colIndex) 0

            else
                -- Checking the next column (only colIndex changes).
                getDisplayEvents evList dEvList grid (colIndex + 1)

        [] ->
            ( dEvList, grid )


gridSetEvent : DisplayEvent -> Matrix Int -> Int -> Matrix Int
gridSetEvent (DisplayEvent id _ dInfo) grid colIndex =
    let
        linesToFill =
            List.range dInfo.lineStart (dInfo.lineEnd - 1)
    in
    -- Writing Event ID from grid[lineStart][colIndex] to grid[lineEnd][colIndex]
    List.foldl (fillLinesGrid id colIndex) grid linesToFill


fillLinesGrid : Int -> Int -> Int -> Matrix Int -> Matrix Int
fillLinesGrid id col line grid =
    Matrix.set grid line col id


fillColsGrid : Int -> Int -> Int -> Matrix Int -> Matrix Int
fillColsGrid id line col grid =
    Matrix.set grid line col id
