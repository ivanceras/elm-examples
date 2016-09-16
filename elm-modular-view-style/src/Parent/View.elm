module Parent.View exposing (view)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Css exposing (..)

import Style exposing (styles,truste_header)
import Parent exposing (..)
import Widget.View
import FontAwesome exposing (..)
import Color


view: Parent.Model -> Html Parent.Msg



view model =
  div []
    [ button [ onClick Decrement ] [ Html.text "-" ]
    , if model.id > 5 then div [] [ Html.text (toString model) ]
      else Html.text "No id for you!..."
    , button [ onClick Increment ] [ Html.text "+" ]
    , div [ styles [ 
                    width (px 100)
                    ,height (px 100)
                    ,backgroundColor (rgb 100 10 10)
                    ,color (rgb 200 10 100)
                ] 
              ,style [("color", "#cf9")]
              ,truste_header
           ]
        [ Html.text "PARENT WIDGET" ]
    , android (Color.rgb 30 30 30) 50
    , check (Color.rgb 0 255 0) 80
    , App.map UpdateWidget (Widget.View.view model.widget)
    ]


