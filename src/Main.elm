module Main exposing (..)

import Browser
import RenderMain.Msg exposing (Msg(..))
import RenderMain.Model exposing (Model(..), init)
import RenderMain.View exposing(view)
import RenderMain.Update exposing (update)
import RenderMain.Subscription exposing (subscriptions)

-- INFO: Hash function = (hour-8)*2+V(minute), V(minute) = 1 if minute >= 30, otherwise minute = 0. type alias Hashmap = Array (List Event).

---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
