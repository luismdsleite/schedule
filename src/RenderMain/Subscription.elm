module RenderMain.Subscription exposing (..)
import RenderMain.Model exposing (Model(..))
import RenderMain.Msg exposing (..)


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ draggable) =
    dnd.subscriptions draggable
