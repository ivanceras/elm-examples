module Style exposing (..)

import Html.Attributes exposing (style)
import Css exposing (..)

styles =
    Css.asPairs >> style
