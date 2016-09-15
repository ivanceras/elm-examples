
module Widget.View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)

import Css exposing (..)
import Style exposing (styles)
import Widget





view: Widget.Model -> Html Widget.Msg
view model =
  div []
      [
      Html.text "This is a child widget"
      ]
