module Child exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)




-- MODEL


type alias Model =
    { position : Position
    , drag : Maybe Drag
    , id : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init :Int -> Position -> Model
init id position =
  ( Model position Nothing id)



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position

type OutMsg =
    ChildListened Int
    | ChildStopListened Int

update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    Debug.log ("Received "++(toString msg)++" in "++(toString model.id))
  ( updateHelp msg model)


updateHelp : Msg -> Model -> (Model, Maybe OutMsg)
updateHelp msg ({position, drag, id} as model) =
  case msg of
    DragStart xy ->
      (Model position (Just (Drag xy xy)) id
      , Just (ChildListened id)
      )

    DragAt xy ->
      (Model position (Maybe.map (\{start} -> Drag start xy) drag) id
      , Nothing
      )

    DragEnd _ ->
      (Model (getPosition model) Nothing id
      , Just (ChildStopListened id)
      )





-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  let
    realPosition =
      getPosition model
  in
    div
      [ onMouseDown
      , style
          [ "background-color" => "#3C8D2F"
          , "cursor" => "move"

          , "width" => "100px"
          , "height" => "100px"
          , "border-radius" => "4px"
          , "position" => "absolute"
          , "left" => px realPosition.x
          , "top" => px realPosition.y

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ text "Drag Me!"
      ]


px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
