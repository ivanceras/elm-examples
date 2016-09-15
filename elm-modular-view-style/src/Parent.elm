module Parent exposing(..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes

import Css exposing (..)


import Widget


type Msg 
    = Increment 
    | Decrement
    | UpdateWidget Widget.Msg

type alias Model = 
    { id: Int
    , message: String
    , widget: Widget.Model
    }


create: Model
create=
    {id = 0
    ,message = "Hello"
    ,widget = Widget.create
    }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({model | id = model.id + 1}
      , Cmd.none)

    Decrement ->
      ({model | id = model.id- 1}
      , Cmd.none)

    UpdateWidget widgetMsg ->
      (model
      , Cmd.none)


init: (Model, Cmd Msg)
init = 
    (create
    ,Cmd.batch[
              ])


subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch []
