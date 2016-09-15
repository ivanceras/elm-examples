
import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json


main =
  beginnerProgram { model = 0, view = view, update = update }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ,div [class "container"
         ,style [("background-color", "#eee")
                ,("width", "500px")
                ,("height", "500px")
                ,("overflow", "auto")
                ]
          ,onScrollTop ScrolledTop
         ]
         [ div [class "widget"
                ,style [("background-color", "#cf9")
                        ,("width", "200px")
                        ,("height", "700px")
                        ]
              ]
           [text "Scrollers"]
        ] 
    ]


type Msg 
    = Increment 
    | Decrement
    | ScrolledTop Int


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    ScrolledTop top ->
      Debug.log ("scrolled "++(toString msg))
      model


targetScrollTop : Json.Decoder Int
targetScrollTop =
  Json.at ["target", "scrollTop"] Json.int

onScrollTop : (Int -> msg) -> Attribute msg
onScrollTop tagger =
  on "scroll" (Json.map tagger targetScrollTop)
