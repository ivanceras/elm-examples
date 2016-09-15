module Parent.View exposing (view)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Css exposing (..)

import Style exposing (styles)
import Parent exposing (..)
import Widget.View
import FontAwesome exposing (..)
import Color




view: Parent.Model -> Html Parent.Msg
view model =
  div []
    [ button [ onClick Decrement ] [ Html.text "-" ]
    , div [] [ Html.text (toString model) ]
    , button [ onClick Increment ] [ Html.text "+" ]
    , div [ styles [ 
                    width (px 100)
                    ,height (px 100)
                    ,backgroundColor (rgb 100 10 10)
                    ,color (rgb 200 10 100)
                ] ]
        [ Html.text "Whexxxxe!<button>oi!</button>" ]
    , android (Color.rgb 30 30 30) 50
    , App.map UpdateWidget (Widget.View.view model.widget)
    ]


