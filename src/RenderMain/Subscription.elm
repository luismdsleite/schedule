module RenderMain.Subscription exposing (..)

import RenderMain.Model exposing (Model)
import RenderMain.Msg exposing (Msg, dnd)


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable
