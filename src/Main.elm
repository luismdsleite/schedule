module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import ScheduleObjects exposing (..)
import Table exposing (..)
import Time exposing (..)



-- TODO: Replace Lists with dictionaries (to search by id)
-- TODO: Add missing data in renderEvent
-- TODO: Remove data in init and read from a json file
-- TODO: Fix names, comment css code
---- PROF  ----
-- TODO: Replace Int for RoomID as that we get a rooms: Dict RoomID Room
-- type alias Model =
--     { rooms : Dict RoomID Room
--     , lecturers : Dict Lecturer
--     , events : List Event
--     , blocks : List Block
--     }
-- TODO: replace weekTime with {Time.Weekday, Time.Hour, Time.Minute}
-- type alias weektime =
--     { weekday : Time.Weekday, Time.Hour : Int, minute : Int }
-- TODO: no bloco representar os events com um condição
---- MODEL ----


type alias Model =
    { rooms : Table Room
    , lecturers : Table Lecturer
    , events : List Event
    , blocks : List Block
    }


init : ( Model, Cmd Msg )
init =
    ( { rooms = fromList [ Room "DCC Lab. 2" "FC6_157 (Lab2)" 20, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30, Room "CCC Lab. 6" "FC6_177 (Lab3)(LongName)" 30 ]
      , lecturers = fromList [ Lecturer "N'Golo Kanté" "NGK" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] [], Lecturer "Alberto" "Al" [] [] []]
      , events =
            [ Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just (ID 0)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 0))
            , Event "asdasd (CC4010)_TP.1" "Alga-TP2" (Just (ID 0)) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) (Just (ID 1))
            ]
      , blocks = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is workng!" ]
        , div [ class "listbox-area" ]
            [ renderLecturers model.lecturers
            , renderEvents model.events model.rooms model.lecturers
            , renderRooms model.rooms
            ]
        ]


renderList : List String -> Html msg
renderList lst =
    ul [ class "list custom-scrollbar" ] (List.map (\l -> li [ class "list-item" ] [ text l ]) lst)


renderEvents : List Event -> Table Room -> Table Lecturer -> Html msg
renderEvents events rooms lecturers =
    ul [ class "list custom-scrollbar" ]
        (List.map (renderEvent rooms lecturers) events)


{-| Transforms an event into a list item
-}
renderEvent : Table Room -> Table Lecturer -> Event -> Html msg
renderEvent rooms lecturers event =
    let
        room =
            case event.room of
                Just id ->
                    case Table.get id rooms of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Room "----" "----" -1

                -- Event still has no room assigned
                Nothing ->
                    Room "----" "----" -1

        lecturer =
            case event.lecturer of
                Just id ->
                    case Table.get id lecturers of
                        Just val ->
                            val

                        -- ERROR: RoomID is missing from the database!
                        Nothing ->
                            Lecturer "----" "----" [] [] []

                -- Event still has no room assigned
                Nothing ->
                    Lecturer "----" "----" [] [] []
    in
    li [ class "list-item" ]
        [ div [ style "width" "10%" ] [ text event.subjectAbbr ]
        , div [ style "width" "35%" ] [ text event.subject ]
        , div [ style "width" "5%" ] [ text (convertWeekDay event.start_time) ]
        , div [ style "width" "10%" ] [ text (convertHourAndMinute event.start_time) ]
        , div [ style "width" "10%" ] [ text (convertHourAndMinute event.end_time) ]
        , div [ style "width" "10%" ] [ text room.abbr ]
        , div [ style "width" "10%" ] [ text (String.fromInt room.capacity) ]
        , div [ style "width" "10%" ] [ text lecturer.abbr ]
        ]


renderRooms : Table Room -> Html msg
renderRooms rooms =
    let
        roomsList =
            Table.toList rooms
    in
    ul [ class "list custom-scrollbar" ]
        (List.map renderRoom roomsList)

renderRoom : ( Int, Room ) -> Html unknown
renderRoom ( int, room ) =
    li [ class "list-item" ] [ div [] [ text room.abbr ] ]

renderLecturers : Table Lecturer -> Html msg
renderLecturers lecturers = 
    let
        renderLecturer ( int, lecturer ) =
            li [ class "list-item" ] [ div [] [ text lecturer.abbr ] ]
        lecturersList = 
            Table.toList lecturers
    in
        ul [ class "list custom-scrollbar" ]
            (List.map renderLecturer lecturersList)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- createRoomID : Int -> RoomID
-- createRoomID int = RoomID(int)
