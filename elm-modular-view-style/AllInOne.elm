
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes

import Css exposing (..)


main =
  beginnerProgram { model = 0, view = view, update = update }

styles =
    Css.asPairs >> Html.Attributes.style


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
        [ Html.text "Whee!" ]
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
