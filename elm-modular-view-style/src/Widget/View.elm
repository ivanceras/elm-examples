
module Widget.View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)

import Css exposing (..)
import Style exposing (styles)
import Widget
import Color





view: Widget.Model -> Html Widget.Msg
view model =
  div [ styles [border3 (px 1) solid (hex "cf9")]]
      [
      Html.text "This is a child widget"
      ]
