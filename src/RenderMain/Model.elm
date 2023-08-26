module RenderMain.Model exposing (Model(..), init)

-- import Dict

import Effect exposing (Effect)
import RenderMain.Msg exposing (Draggable, Msg(..), dnd)
import ScheduleObjects.Data exposing (Data)
import ScheduleObjects.Event exposing (Event, EventID)
import ScheduleObjects.Filters exposing (ScheduleFilter)



-- import ScheduleObjects.Block exposing (Block)
-- import ScheduleObjects.Event exposing (Event)
-- import ScheduleObjects.Lecturer exposing (Lecturer)
-- import ScheduleObjects.Room exposing (Room)
-- import ScheduleObjects.WeekTime exposing (WeekTime)
-- import Time


type Model
    = Model Data ScheduleFilter Draggable ( EventID, Event )


init : Data -> () -> ( Model, Effect Msg )
init data () =
    ( Model data
        (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) "" "" "")
        dnd.model
        ( -1, Event "" "" Nothing Nothing Nothing Nothing )
    , Effect.none
    )



-- init : Data -> ( Model, Effect Msg )
-- init _ =
--     ( Model
--         { rooms =
--             Dict.fromList
--                 [ ( 1, Room "DCC Lab. 2" "FC6_157 (Lab2)" 20 "1.58" )
--                 , ( 2, Room "DCC Lab. 3" "FC6_177 (Lab3)" 30 "1.93" )
--                 , ( 3, Room "CCC Lab. 6" "FC2_222 (Lab3)(LongName)" 30 "1.67" )
--                 ]
--         , lecturers =
--             Dict.fromList
--                 [ ( 1, Lecturer "N'Golo Kanté" "NGK" [] [] [] "1.55" )
--                 , ( 2, Lecturer "Alberto" "Al" [] [] [] "1.28" )
--                 , ( 3, Lecturer "Apikalia" "Ak" [] [] [] "1.66" )
--                 , ( 4, Lecturer "Sofi" "Ae" [] [] [] "1.73" )
--                 , ( 5, Lecturer "Lianne" "Ac" [] [] [] "1.54" )
--                 , ( 6, Lecturer "Mayur" "Aa" [] [] [] "1.42" )
--                 , ( 7, Lecturer "Kristine" "Af" [] [] [] "1.83" )
--                 , ( 8, Lecturer "Straton" "Az" [] [] [] "1.69" )
--                 , ( 9, Lecturer "Svanhild" "Am" [] [] [] "1.93" )
--                 , ( 10, Lecturer "Ayla" "Aç" [] [] [] "1.44" )
--                 , ( 11, Lecturer "Mayem" "Ai" [] [] [] "1.62" )
--                 , ( 12, Lecturer "Minakshi" "BA" [] [] [] "1.03" )
--                 , ( 13, Lecturer "Isaiah" "CA" [] [] [] "1.11" )
--                 ]
--         , events =
--             Dict.fromList
--                 [ ( 1, Event "Algoritmos (CC4010)_TP.1" "Alga-TP3" (Just 1) (Just 1) (Just (WeekTime Time.Mon 9 30)) (Just (WeekTime Time.Mon 11 0)) )
--                 , ( 2, Event "asdasd (CC4011)_TP.1" "Alga-TP2" (Just 1) (Just 2) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) )
--                 , ( 3, Event "asdasd (CC4011)_TP.1" "Alga-TP45" (Just 1) (Just 2) (Just (WeekTime Time.Mon 10 30)) (Just (WeekTime Time.Mon 12 0)) )
--                 , ( 4, Event "asdasd (CC4011)_TP.1" "Alga-TP44" (Just 1) (Just 2) (Just (WeekTime Time.Mon 12 30)) (Just (WeekTime Time.Mon 13 30)) )
--                 , ( 5, Event "Harooo" "Alga-TPX" (Just 1) (Just 2) (Just (WeekTime Time.Mon 11 0)) (Just (WeekTime Time.Mon 14 0)) )
--                 , ( 6, Event "Harooo" "Alga-TPY" (Just 1) (Just 2) (Just (WeekTime Time.Mon 15 0)) (Just (WeekTime Time.Mon 17 0)) )
--                 , ( 7, Event "subject" "subjAbrr" (Just 2) (Just 2) (Just (WeekTime Time.Mon 11 30)) (Just (WeekTime Time.Mon 12 30)) )
--                 , ( 8, Event "noRoomEvent" "noRoomEvent" Nothing (Just 2) (Just (WeekTime Time.Tue 11 30)) (Just (WeekTime Time.Tue 12 0)) )
--                 , ( 9, Event "noLectEvent" "noLectEvent" (Just 2) Nothing (Just (WeekTime Time.Sat 11 30)) (Just (WeekTime Time.Sat 12 0)) )
--                 , ( 10, Event "noRoom&LecEvent" "noRoom&LecEvent" Nothing Nothing (Just (WeekTime Time.Wed 9 30)) (Just (WeekTime Time.Wed 12 0)) )
--                 ]
--         , blocks =
--             Dict.fromList
--                 [ ( 1, Block "All Events" "All Events" (\_ -> True) )
--                 , ( 2, Block "Eventos de Alga" "(CC4011)" (\ev -> String.contains "(CC4011)" ev.subject) )
--                 ]
--         }
--         (ScheduleFilter (\_ _ -> False) (\_ _ -> False) (\_ _ -> False) "" "" "")
--         dnd.model
--     , Effect.none
--     )
