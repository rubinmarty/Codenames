module App exposing (app)

import Navigation
import State exposing (init, update, subscriptions)
import Types exposing (Msg(UrlChange))
import View exposing (view)


app =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
