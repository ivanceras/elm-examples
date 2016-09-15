
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Child


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
    { child : List Child.Model
    , listened: Maybe Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model 
        [Child.init 0 (Position 200 200)
        ,Child.init 1 (Position 300 300)
        ]
        Nothing

   , Cmd.none )



-- UPDATE


type Msg
    =  UpdateChild Int Child.Msg
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateChild childId childMsg ->
            updateThenHandleChildMsg childMsg childId model

        DragAt position ->
            case model.listened of
                Just id ->
                    updateThenHandleChildMsg (Child.DragAt position) id model
                Nothing ->
                    (model, Cmd.none)

        DragEnd position ->
             case model.listened of
                Just id ->
                    updateThenHandleChildMsg (Child.DragEnd position) id model
                Nothing ->
                    (model, Cmd.none)

updateThenHandleChildMsg: Child.Msg -> Int -> Model -> (Model, Cmd Msg)
updateThenHandleChildMsg childMsg childId model =
    updateChild childMsg childId model
        |> handleChildOutMsg

handleChildOutMsg: (Model, Maybe Child.OutMsg) -> (Model, Cmd Msg)
handleChildOutMsg (model, outmsg) =
    case outmsg of
        Just outmsg ->
            case outmsg of
                Child.ChildListened id ->
                    ({model | listened = Just id}
                    , Cmd.none)

                Child.ChildStopListened id ->
                    ({model | listened = Nothing }
                    , Cmd.none)

        Nothing ->
            (model, Cmd.none)
        

updateChild: Child.Msg -> Int ->  Model -> (Model, Maybe Child.OutMsg)
updateChild childMsg childId model =
    let children =
            List.map(
                \child ->
                    if child.id == childId then
                        Child.update childMsg child
                    else
                        (child, Nothing)
              ) model.child
              
        (child, outmsgs) = List.unzip children
    in
    ({model | child = child
    }
    ,
    (List.filterMap (
            \o -> o
        ) outmsgs
         |> List.head
    )
    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.listened of
        Just id ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
        Nothing ->
            Sub.none



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
    div []
        <| List.map
            (\ child -> 
                App.map (UpdateChild child.id) (Child.view child)
            ) model.child


