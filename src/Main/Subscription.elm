module Main.Subscription exposing (..)

import Main.Model exposing (Model)
import Main.Msg exposing (Msg, dnd)


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable
