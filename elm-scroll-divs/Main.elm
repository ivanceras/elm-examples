
import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))


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
          ,onScroll Scrolled
         ]
         [ div [class "widget"
                ,style [("background-color", "#cf9")
                        ,("width", "1200px")
                        ,("height", "700px")
                        ]
              ]
           [text "Scrollers"]
        ] 
    ]


type Msg 
    = Increment 
    | Decrement
    | Scrolled Scroll


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    Scrolled scroll ->
      Debug.log ("scrolled "++(toString msg))
      model


type alias Scroll =
 {left: Int
  ,top: Int
 }

scrollDecoder: Json.Decoder Scroll
scrollDecoder =
    Json.object2 Scroll 
         ("scrollTop" := Json.int)
         ("scrollLeft" := Json.int)



targetScroll : Json.Decoder Scroll
targetScroll =
  Json.at ["target"] scrollDecoder 

onScroll : (Scroll -> msg) -> Attribute msg
onScroll tagger =
  on "scroll" (Json.map tagger targetScroll)
