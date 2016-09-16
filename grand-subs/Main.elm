import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
    { childmodel : ChildModel }

type alias ChildModel =
    { position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model <| ChildModel (Position 200 200) Nothing, Cmd.none )



-- UPDATE


type ChildMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position

type Msg 
    = ChildMsg ChildMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChildMsg msg ->
      let (childmodel, childmsg) = childupdate msg model.childmodel
      in
        (Model childmodel, Cmd.map ChildMsg childmsg)

childupdate : ChildMsg -> ChildModel -> ( ChildModel, Cmd ChildMsg )
childupdate msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : ChildMsg -> ChildModel -> ChildModel
updateHelp msg ({position, drag} as model) =
  case  msg of
    DragStart xy ->
      ChildModel position (Just (Drag xy xy))

    DragAt xy ->
      ChildModel position (Maybe.map (\{start} -> Drag start xy) drag)

    DragEnd _ ->
      ChildModel (getPosition model) Nothing



-- SUBSCRIPTIONS


childsubs : ChildModel -> Sub ChildMsg
childsubs model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map ChildMsg (childsubs model.childmodel)

-- VIEW


(=>) = (,)

view : Model -> Html Msg
view model =
  App.map ChildMsg (childview model.childmodel)

childview : ChildModel -> Html ChildMsg
childview model =
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


getPosition : ChildModel -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Attribute ChildMsg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
