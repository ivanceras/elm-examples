module Style exposing (..)

import Html.Attributes exposing (style)
import Css exposing (..)

styles =
    Css.asPairs >> style


truste_header = 
    styles [ border3 (px 1) solid (rgb 10 10 255)]
