module Main exposing (..)

import Browser
import RenderMain.Model exposing (Model(..), init)
import RenderMain.Msg exposing (Msg(..))
import RenderMain.Subscription exposing (subscriptions)
import RenderMain.Update exposing (update)
import RenderMain.View exposing (view)



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
