module Pages.Main exposing (Model, Msg, page)

import Gen.Params.Main exposing (Params)
import Html
import Page
import RenderMain.Model exposing (init)
import RenderMain.Msg
import RenderMain.Subscription exposing (subscriptions)
import RenderMain.Update exposing (update)
import RenderMain.View exposing (view)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = generateHtml "Main" view
        , subscriptions = subscriptions
        }


generateHtml : String -> (Model -> Html.Html Msg) -> Model -> View Msg
generateHtml title view model =
    { title = title
    , body = [ view model ]
    }


type alias Msg =
    RenderMain.Msg.Msg


type alias Model =
    RenderMain.Model.Model
